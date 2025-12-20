module atlas_mod
  use, intrinsic :: iso_c_binding
  use glyph_mod
  use font_mod
  use gl_bindings
  implicit none
  private

  public :: atlas_t
  public :: atlas_create, atlas_destroy, atlas_get_glyph

  integer, parameter :: ATLAS_SIZE = 1024  ! Larger for Unicode support
  integer, parameter :: ASCII_START = 32
  integer, parameter :: ASCII_END = 126
  integer, parameter :: EXTENDED_CACHE_SIZE = 256  ! Cache for non-ASCII glyphs

  ! Entry for extended glyph cache (simple hash table)
  type :: cache_entry_t
    integer :: codepoint = 0
    type(glyph_t) :: glyph
    logical :: used = .false.
  end type cache_entry_t

  type :: atlas_t
    integer :: texture_id = 0
    integer :: width = ATLAS_SIZE
    integer :: height = ATLAS_SIZE
    integer :: cursor_x = 0
    integer :: cursor_y = 0
    integer :: row_height = 0
    type(glyph_t) :: glyphs(0:127)              ! ASCII cache
    type(cache_entry_t) :: extended(EXTENDED_CACHE_SIZE)  ! Extended glyph cache
    type(font_t), pointer :: font => null()     ! Reference for on-demand loading
    logical :: initialized = .false.
  end type atlas_t

contains

  ! Create a texture atlas from a font, pre-rendering ASCII characters
  function atlas_create(font) result(atlas)
    type(font_t), intent(inout), target :: font
    type(atlas_t) :: atlas
    integer :: tex(1)
    integer :: cp, i
    type(glyph_t) :: g
    type(c_ptr) :: bitmap_ptr
    integer(c_int8_t), target :: white_pixel(1)

    if (.not. font%loaded) then
      print *, "Error: Cannot create atlas from unloaded font"
      return
    end if

    ! Store font reference for on-demand glyph loading
    atlas%font => font

    ! Create OpenGL texture
    call glGenTextures(1, tex)
    atlas%texture_id = tex(1)
    call glBindTexture(GL_TEXTURE_2D, atlas%texture_id)

    ! Set texture parameters
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    call glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)

    ! Disable byte-alignment restriction for grayscale textures
    call glPixelStorei(GL_UNPACK_ALIGNMENT, 1)

    ! Allocate empty texture
    call glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, &
                      atlas%width, atlas%height, 0, &
                      GL_RED, GL_UNSIGNED_BYTE, c_null_ptr)

    ! Add a solid white pixel at position (0,0) for solid rectangle rendering
    ! This allows renderer_draw_rect to sample a non-transparent pixel
    white_pixel(1) = -1_c_int8_t  ! 255 as signed byte
    call glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 1, 1, &
                         GL_RED, GL_UNSIGNED_BYTE, c_loc(white_pixel))

    ! Initialize glyph array
    do cp = 0, 127
      call glyph_init(atlas%glyphs(cp))
    end do

    ! Initialize extended cache
    do i = 1, EXTENDED_CACHE_SIZE
      atlas%extended(i)%used = .false.
      atlas%extended(i)%codepoint = 0
      call glyph_init(atlas%extended(i)%glyph)
    end do

    ! Pre-render ASCII printable characters (32-126)
    atlas%cursor_x = 1
    atlas%cursor_y = 1
    atlas%row_height = 0

    do cp = ASCII_START, ASCII_END
      g = font_render_glyph(font, cp, bitmap_ptr)
      if (g%valid) then
        call atlas_add_glyph(atlas, g, bitmap_ptr)
        atlas%glyphs(cp) = g
      end if
    end do

    ! Unbind texture
    call glBindTexture(GL_TEXTURE_2D, 0)

    atlas%initialized = .true.

  end function atlas_create

  ! Add a rendered glyph to the atlas
  subroutine atlas_add_glyph(atlas, g, bitmap_ptr)
    type(atlas_t), intent(inout) :: atlas
    type(glyph_t), intent(inout) :: g
    type(c_ptr), intent(in) :: bitmap_ptr
    integer :: padding

    padding = 1  ! 1 pixel padding between glyphs

    ! Check if glyph fits in current row
    if (atlas%cursor_x + g%width + padding > atlas%width) then
      ! Move to next row
      atlas%cursor_x = 1
      atlas%cursor_y = atlas%cursor_y + atlas%row_height + padding
      atlas%row_height = 0
    end if

    ! Check if glyph fits vertically
    if (atlas%cursor_y + g%height + padding > atlas%height) then
      print *, "Warning: Atlas texture is full, cannot add glyph"
      return
    end if

    ! Store position in atlas
    g%tex_x = atlas%cursor_x
    g%tex_y = atlas%cursor_y

    ! Copy bitmap to texture
    if (g%width > 0 .and. g%height > 0 .and. c_associated(bitmap_ptr)) then
      call glTexSubImage2D(GL_TEXTURE_2D, 0, &
                           g%tex_x, g%tex_y, g%width, g%height, &
                           GL_RED, GL_UNSIGNED_BYTE, bitmap_ptr)
    end if

    ! Calculate UV coordinates (normalized 0-1)
    g%u0 = real(g%tex_x, c_float) / real(atlas%width, c_float)
    g%v0 = real(g%tex_y, c_float) / real(atlas%height, c_float)
    g%u1 = real(g%tex_x + g%width, c_float) / real(atlas%width, c_float)
    g%v1 = real(g%tex_y + g%height, c_float) / real(atlas%height, c_float)

    ! Update cursor position
    atlas%cursor_x = atlas%cursor_x + g%width + padding
    atlas%row_height = max(atlas%row_height, g%height)

  end subroutine atlas_add_glyph

  ! Get glyph information for a codepoint (with on-demand loading for non-ASCII)
  function atlas_get_glyph(atlas, codepoint) result(g)
    type(atlas_t), intent(inout) :: atlas
    integer, intent(in) :: codepoint
    type(glyph_t) :: g

    if (codepoint >= 0 .and. codepoint <= 127) then
      g = atlas%glyphs(codepoint)
    else
      ! Look up or load extended glyph
      g = atlas_get_extended_glyph(atlas, codepoint)
    end if
  end function atlas_get_glyph

  ! Get or load an extended (non-ASCII) glyph
  function atlas_get_extended_glyph(atlas, codepoint) result(g)
    type(atlas_t), intent(inout) :: atlas
    integer, intent(in) :: codepoint
    type(glyph_t) :: g
    integer :: hash_idx, probe, i
    type(c_ptr) :: bitmap_ptr
    logical :: used_fallback

    call glyph_init(g)

    ! Simple hash function
    hash_idx = mod(codepoint, EXTENDED_CACHE_SIZE) + 1

    ! Linear probe to find existing or empty slot
    do i = 0, EXTENDED_CACHE_SIZE - 1
      probe = mod(hash_idx + i - 1, EXTENDED_CACHE_SIZE) + 1

      if (.not. atlas%extended(probe)%used) then
        ! Empty slot - load glyph here
        call atlas_load_extended_glyph(atlas, codepoint, probe)
        g = atlas%extended(probe)%glyph
        return
      else if (atlas%extended(probe)%codepoint == codepoint) then
        ! Found cached glyph
        g = atlas%extended(probe)%glyph
        return
      end if
    end do

    ! Cache is full - just render without caching (fallback behavior)
    if (associated(atlas%font)) then
      g = font_render_glyph_with_fallback(atlas%font, codepoint, bitmap_ptr, used_fallback)
    end if

  end function atlas_get_extended_glyph

  ! Load an extended glyph into the cache at specified slot
  subroutine atlas_load_extended_glyph(atlas, codepoint, slot)
    type(atlas_t), intent(inout) :: atlas
    integer, intent(in) :: codepoint, slot
    type(glyph_t) :: g
    type(c_ptr) :: bitmap_ptr
    logical :: used_fallback

    if (.not. associated(atlas%font)) return

    ! Render using fallback support
    g = font_render_glyph_with_fallback(atlas%font, codepoint, bitmap_ptr, used_fallback)

    if (g%valid) then
      ! Bind texture and add glyph
      call glBindTexture(GL_TEXTURE_2D, atlas%texture_id)
      call atlas_add_glyph(atlas, g, bitmap_ptr)
      call glBindTexture(GL_TEXTURE_2D, 0)

      ! Store in cache
      atlas%extended(slot)%codepoint = codepoint
      atlas%extended(slot)%glyph = g
      atlas%extended(slot)%used = .true.
    end if

  end subroutine atlas_load_extended_glyph

  ! Destroy atlas and free texture
  subroutine atlas_destroy(atlas)
    type(atlas_t), intent(inout) :: atlas
    integer :: tex(1)

    if (atlas%texture_id > 0) then
      tex(1) = atlas%texture_id
      call glDeleteTextures(1, tex)
      atlas%texture_id = 0
    end if

    atlas%initialized = .false.
  end subroutine atlas_destroy

end module atlas_mod
