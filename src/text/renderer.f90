module renderer_mod
  use, intrinsic :: iso_c_binding
  use gl_bindings
  use glyph_mod
  use font_mod
  use atlas_mod
  use shader_mod
  implicit none
  private

  public :: renderer_t
  public :: renderer_create, renderer_destroy
  public :: renderer_begin, renderer_draw_char, renderer_draw_string, renderer_flush
  public :: renderer_set_projection, renderer_load_fallback_font

  ! Vertex format: position(2) + texcoord(2) + color(4) = 8 floats per vertex
  integer, parameter :: FLOATS_PER_VERTEX = 8
  integer, parameter :: VERTICES_PER_QUAD = 6
  integer, parameter :: FLOATS_PER_QUAD = FLOATS_PER_VERTEX * VERTICES_PER_QUAD
  integer, parameter :: MAX_QUADS = 4096
  integer, parameter :: MAX_VERTICES = MAX_QUADS * VERTICES_PER_QUAD
  integer, parameter :: BUFFER_SIZE = MAX_VERTICES * FLOATS_PER_VERTEX

  type :: renderer_t
    integer :: vao = 0
    integer :: vbo = 0
    real(c_float), allocatable :: vertices(:)
    integer :: vertex_count = 0
    type(shader_t) :: shader
    type(atlas_t) :: atlas
    type(font_t) :: font
    real(c_float) :: projection(4,4)
    logical :: initialized = .false.
  end type renderer_t

  ! Embedded shader sources
  character(len=*), parameter :: VERT_SHADER = &
    "#version 330 core" // c_new_line // &
    "layout(location = 0) in vec2 a_position;" // c_new_line // &
    "layout(location = 1) in vec2 a_texcoord;" // c_new_line // &
    "layout(location = 2) in vec4 a_color;" // c_new_line // &
    "uniform mat4 u_projection;" // c_new_line // &
    "out vec2 v_texcoord;" // c_new_line // &
    "out vec4 v_color;" // c_new_line // &
    "void main() {" // c_new_line // &
    "    gl_Position = u_projection * vec4(a_position, 0.0, 1.0);" // c_new_line // &
    "    v_texcoord = a_texcoord;" // c_new_line // &
    "    v_color = a_color;" // c_new_line // &
    "}"

  character(len=*), parameter :: FRAG_SHADER = &
    "#version 330 core" // c_new_line // &
    "in vec2 v_texcoord;" // c_new_line // &
    "in vec4 v_color;" // c_new_line // &
    "uniform sampler2D u_atlas;" // c_new_line // &
    "out vec4 frag_color;" // c_new_line // &
    "void main() {" // c_new_line // &
    "    float alpha = texture(u_atlas, v_texcoord).r;" // c_new_line // &
    "    frag_color = vec4(v_color.rgb, v_color.a * alpha);" // c_new_line // &
    "}"

contains

  ! Create renderer with font
  function renderer_create(font_path, font_size) result(r)
    character(len=*), intent(in) :: font_path
    integer, intent(in) :: font_size
    type(renderer_t) :: r
    integer :: vao(1), vbo(1)
    integer(c_size_t) :: stride, offset

    ! Load font
    r%font = font_load(font_path, font_size)
    if (.not. r%font%loaded) then
      print *, "Error: Failed to load font"
      return
    end if

    ! Create atlas from font
    r%atlas = atlas_create(r%font)
    if (.not. r%atlas%initialized) then
      print *, "Error: Failed to create atlas"
      call font_destroy(r%font)
      return
    end if

    ! Create shader
    r%shader = shader_create(VERT_SHADER, FRAG_SHADER)
    if (.not. r%shader%valid) then
      print *, "Error: Failed to create shader"
      call atlas_destroy(r%atlas)
      call font_destroy(r%font)
      return
    end if

    ! Create VAO and VBO
    call glGenVertexArrays(1, vao)
    call glGenBuffers(1, vbo)
    r%vao = vao(1)
    r%vbo = vbo(1)

    call glBindVertexArray(r%vao)
    call glBindBuffer(GL_ARRAY_BUFFER, r%vbo)

    ! Allocate buffer (empty for now)
    call glBufferData(GL_ARRAY_BUFFER, &
                      int(BUFFER_SIZE * c_sizeof(1.0_c_float), c_size_t), &
                      c_null_ptr, GL_DYNAMIC_DRAW)

    ! Set up vertex attributes
    stride = int(FLOATS_PER_VERTEX * c_sizeof(1.0_c_float), c_size_t)

    ! Position: location 0, 2 floats, offset 0
    offset = 0_c_size_t
    call glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, int(stride, c_int), offset)
    call glEnableVertexAttribArray(0)

    ! Texcoord: location 1, 2 floats, offset 8 bytes
    offset = int(2 * c_sizeof(1.0_c_float), c_size_t)
    call glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, int(stride, c_int), offset)
    call glEnableVertexAttribArray(1)

    ! Color: location 2, 4 floats, offset 16 bytes
    offset = int(4 * c_sizeof(1.0_c_float), c_size_t)
    call glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, int(stride, c_int), offset)
    call glEnableVertexAttribArray(2)

    call glBindVertexArray(0)

    ! Allocate vertex buffer
    allocate(r%vertices(BUFFER_SIZE))
    r%vertices = 0.0_c_float
    r%vertex_count = 0

    ! Initialize projection to identity
    r%projection = 0.0_c_float
    r%projection(1,1) = 1.0_c_float
    r%projection(2,2) = 1.0_c_float
    r%projection(3,3) = 1.0_c_float
    r%projection(4,4) = 1.0_c_float

    r%initialized = .true.

  end function renderer_create

  ! Destroy renderer
  subroutine renderer_destroy(r)
    type(renderer_t), intent(inout) :: r
    integer :: arr(1)

    if (r%vbo > 0) then
      arr(1) = r%vbo
      call glDeleteBuffers(1, arr)
      r%vbo = 0
    end if

    if (r%vao > 0) then
      arr(1) = r%vao
      call glDeleteVertexArrays(1, arr)
      r%vao = 0
    end if

    call shader_destroy(r%shader)
    call atlas_destroy(r%atlas)
    call font_destroy(r%font)

    if (allocated(r%vertices)) deallocate(r%vertices)
    r%initialized = .false.
  end subroutine renderer_destroy

  ! Begin a new frame (reset vertex buffer)
  subroutine renderer_begin(r)
    type(renderer_t), intent(inout) :: r
    r%vertex_count = 0
  end subroutine renderer_begin

  ! Set orthographic projection matrix
  subroutine renderer_set_projection(r, width, height)
    type(renderer_t), intent(inout) :: r
    integer, intent(in) :: width, height

    ! Orthographic projection: (0,0) top-left, (width, height) bottom-right
    r%projection = 0.0_c_float
    r%projection(1,1) = 2.0_c_float / real(width, c_float)
    r%projection(2,2) = -2.0_c_float / real(height, c_float)  ! Flip Y
    r%projection(3,3) = -1.0_c_float
    r%projection(4,4) = 1.0_c_float
    r%projection(1,4) = -1.0_c_float
    r%projection(2,4) = 1.0_c_float
  end subroutine renderer_set_projection

  ! Draw a single character at position (x, y)
  subroutine renderer_draw_char(r, x, y, codepoint, red, green, blue, alpha)
    type(renderer_t), intent(inout) :: r
    real, intent(in) :: x, y
    integer, intent(in) :: codepoint
    real, intent(in) :: red, green, blue, alpha
    type(glyph_t) :: g
    real(c_float) :: x0, y0, x1, y1
    real(c_float) :: u0, v0, u1, v1
    integer :: base

    if (.not. r%initialized) return

    g = atlas_get_glyph(r%atlas, codepoint)
    if (.not. g%valid) return

    ! Check if buffer is full
    if (r%vertex_count + VERTICES_PER_QUAD > MAX_VERTICES) then
      call renderer_flush(r)
    end if

    ! Calculate quad positions
    ! x, y is the cursor position (baseline)
    x0 = real(x, c_float) + real(g%bearing_x, c_float)
    y0 = real(y, c_float) - real(g%bearing_y, c_float)
    x1 = x0 + real(g%width, c_float)
    y1 = y0 + real(g%height, c_float)

    u0 = g%u0
    v0 = g%v0
    u1 = g%u1
    v1 = g%v1

    ! Add 6 vertices (2 triangles) for the quad
    base = r%vertex_count * FLOATS_PER_VERTEX + 1

    ! Triangle 1: top-left, bottom-left, bottom-right
    ! Vertex 1: top-left
    r%vertices(base:base+7) = [x0, y0, u0, v0, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]
    base = base + 8

    ! Vertex 2: bottom-left
    r%vertices(base:base+7) = [x0, y1, u0, v1, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]
    base = base + 8

    ! Vertex 3: bottom-right
    r%vertices(base:base+7) = [x1, y1, u1, v1, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]
    base = base + 8

    ! Triangle 2: top-left, bottom-right, top-right
    ! Vertex 4: top-left
    r%vertices(base:base+7) = [x0, y0, u0, v0, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]
    base = base + 8

    ! Vertex 5: bottom-right
    r%vertices(base:base+7) = [x1, y1, u1, v1, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]
    base = base + 8

    ! Vertex 6: top-right
    r%vertices(base:base+7) = [x1, y0, u1, v0, &
                               real(red,c_float), real(green,c_float), &
                               real(blue,c_float), real(alpha,c_float)]

    r%vertex_count = r%vertex_count + VERTICES_PER_QUAD

  end subroutine renderer_draw_char

  ! Draw a string at position (x, y)
  subroutine renderer_draw_string(r, x, y, text, red, green, blue, alpha)
    type(renderer_t), intent(inout) :: r
    real, intent(in) :: x, y
    character(len=*), intent(in) :: text
    real, intent(in) :: red, green, blue, alpha
    integer :: i, cp
    real :: cursor_x
    type(glyph_t) :: g

    cursor_x = x
    do i = 1, len_trim(text)
      cp = ichar(text(i:i))
      call renderer_draw_char(r, cursor_x, y, cp, red, green, blue, alpha)

      ! Advance cursor
      g = atlas_get_glyph(r%atlas, cp)
      if (g%valid) then
        cursor_x = cursor_x + real(g%advance)
      end if
    end do
  end subroutine renderer_draw_string

  ! Flush the vertex buffer to GPU and draw
  subroutine renderer_flush(r)
    type(renderer_t), intent(inout) :: r
    integer(c_size_t) :: data_size

    if (r%vertex_count == 0) return

    ! Enable blending for text
    call glEnable(GL_BLEND)
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    ! Bind shader and set uniforms
    call shader_use(r%shader)
    call shader_set_projection(r%shader, r%projection)

    ! Bind texture
    call glActiveTexture(GL_TEXTURE0)
    call glBindTexture(GL_TEXTURE_2D, r%atlas%texture_id)
    call glUniform1i(r%shader%atlas_loc, 0)

    ! Upload vertex data
    call glBindVertexArray(r%vao)
    call glBindBuffer(GL_ARRAY_BUFFER, r%vbo)

    data_size = int(r%vertex_count * FLOATS_PER_VERTEX * c_sizeof(1.0_c_float), c_size_t)
    call glBufferSubData_floats(GL_ARRAY_BUFFER, 0_c_size_t, data_size, r%vertices)

    ! Draw
    call glDrawArrays(GL_TRIANGLES, 0, r%vertex_count)

    ! Unbind
    call glBindVertexArray(0)
    call glBindTexture(GL_TEXTURE_2D, 0)

    ! Reset vertex count
    r%vertex_count = 0

  end subroutine renderer_flush

  ! Load a fallback font for missing glyphs
  subroutine renderer_load_fallback_font(r, font_path)
    type(renderer_t), intent(inout) :: r
    character(len=*), intent(in) :: font_path

    if (.not. r%initialized) return
    call font_load_fallback(r%font, font_path)
  end subroutine renderer_load_fallback_font

end module renderer_mod
