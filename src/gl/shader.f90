module shader_mod
  use, intrinsic :: iso_c_binding
  use gl_bindings
  implicit none
  private

  public :: shader_t
  public :: shader_create, shader_destroy, shader_use
  public :: shader_set_projection, shader_set_int

  type :: shader_t
    integer :: program_id = 0
    integer :: projection_loc = -1
    integer :: atlas_loc = -1
    logical :: valid = .false.
  end type shader_t

contains

  ! Create a shader program from vertex and fragment source
  function shader_create(vert_source, frag_source) result(shader)
    character(len=*), intent(in) :: vert_source, frag_source
    type(shader_t) :: shader
    integer :: vert_id, frag_id
    integer :: success
    character(len=512) :: log_buffer

    ! Compile vertex shader
    vert_id = glCreateShader(GL_VERTEX_SHADER)
    call glShaderSource(vert_id, vert_source // c_null_char)
    call glCompileShader(vert_id)

    success = glGetShaderCompileStatus(vert_id)
    if (success == 0) then
      call glGetShaderInfoLog(vert_id, log_buffer, 512)
      print *, "Error: Vertex shader compilation failed:"
      print *, trim(log_buffer)
      call glDeleteShader(vert_id)
      return
    end if

    ! Compile fragment shader
    frag_id = glCreateShader(GL_FRAGMENT_SHADER)
    call glShaderSource(frag_id, frag_source // c_null_char)
    call glCompileShader(frag_id)

    success = glGetShaderCompileStatus(frag_id)
    if (success == 0) then
      call glGetShaderInfoLog(frag_id, log_buffer, 512)
      print *, "Error: Fragment shader compilation failed:"
      print *, trim(log_buffer)
      call glDeleteShader(vert_id)
      call glDeleteShader(frag_id)
      return
    end if

    ! Link program
    shader%program_id = glCreateProgram()
    call glAttachShader(shader%program_id, vert_id)
    call glAttachShader(shader%program_id, frag_id)
    call glLinkProgram(shader%program_id)

    success = glGetProgramLinkStatus(shader%program_id)
    if (success == 0) then
      call glGetProgramInfoLog(shader%program_id, log_buffer, 512)
      print *, "Error: Shader program linking failed:"
      print *, trim(log_buffer)
      call glDeleteShader(vert_id)
      call glDeleteShader(frag_id)
      call glDeleteProgram(shader%program_id)
      shader%program_id = 0
      return
    end if

    ! Clean up shaders (they're linked into the program now)
    call glDeleteShader(vert_id)
    call glDeleteShader(frag_id)

    ! Get uniform locations
    shader%projection_loc = glGetUniformLocation(shader%program_id, &
                                                  "u_projection" // c_null_char)
    shader%atlas_loc = glGetUniformLocation(shader%program_id, &
                                             "u_atlas" // c_null_char)

    shader%valid = .true.

  end function shader_create

  ! Use this shader program
  subroutine shader_use(shader)
    type(shader_t), intent(in) :: shader
    if (shader%valid) then
      call glUseProgram(shader%program_id)
    end if
  end subroutine shader_use

  ! Set the projection matrix uniform
  subroutine shader_set_projection(shader, matrix)
    type(shader_t), intent(in) :: shader
    real(c_float), intent(in) :: matrix(4,4)

    if (shader%valid .and. shader%projection_loc >= 0) then
      call glUniformMatrix4fv(shader%projection_loc, 1, GL_FALSE, matrix)
    end if
  end subroutine shader_set_projection

  ! Set an integer uniform
  subroutine shader_set_int(shader, name, value)
    type(shader_t), intent(in) :: shader
    character(len=*), intent(in) :: name
    integer, intent(in) :: value
    integer :: loc

    if (shader%valid) then
      loc = glGetUniformLocation(shader%program_id, name // c_null_char)
      if (loc >= 0) then
        call glUniform1i(loc, value)
      end if
    end if
  end subroutine shader_set_int

  ! Destroy shader program
  subroutine shader_destroy(shader)
    type(shader_t), intent(inout) :: shader

    if (shader%program_id > 0) then
      call glDeleteProgram(shader%program_id)
      shader%program_id = 0
    end if
    shader%valid = .false.
  end subroutine shader_destroy

end module shader_mod
