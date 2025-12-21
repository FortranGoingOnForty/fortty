module font_mod
  use, intrinsic :: iso_c_binding
  use glyph_mod
  implicit none
  private

  public :: font_t
  public :: font_load, font_destroy, font_render_glyph
  public :: font_load_fallback, font_has_glyph, font_render_glyph_with_fallback
  public :: font_find_for_codepoint, font_find_monospace

  ! Font handle and metrics
  type :: font_t
    type(c_ptr) :: ft_library = c_null_ptr
    type(c_ptr) :: ft_face = c_null_ptr
    type(c_ptr) :: ft_face_fallback = c_null_ptr
    integer :: size_px = 0
    integer :: cell_width = 0
    integer :: cell_height = 0
    integer :: ascender = 0
    integer :: descender = 0
    integer :: fallback_ascender = 0  ! For baseline alignment with fallback font
    logical :: loaded = .false.
    logical :: has_fallback = .false.
  end type font_t

  ! C interface to FreeType helpers
  interface
    integer(c_int) function fortty_ft_init(lib) bind(C, name="fortty_ft_init")
      import :: c_int, c_ptr
      type(c_ptr), intent(out) :: lib
    end function fortty_ft_init

    subroutine fortty_ft_done(lib) bind(C, name="fortty_ft_done")
      import :: c_ptr
      type(c_ptr), value :: lib
    end subroutine fortty_ft_done

    integer(c_int) function fortty_ft_load_font(lib, path, size_px, face) &
        bind(C, name="fortty_ft_load_font")
      import :: c_int, c_ptr, c_char
      type(c_ptr), value :: lib
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), value :: size_px
      type(c_ptr), intent(out) :: face
    end function fortty_ft_load_font

    subroutine fortty_ft_done_face(face) bind(C, name="fortty_ft_done_face")
      import :: c_ptr
      type(c_ptr), value :: face
    end subroutine fortty_ft_done_face

    subroutine fortty_ft_get_metrics(face, cell_width, cell_height, &
                                      ascender, descender) &
        bind(C, name="fortty_ft_get_metrics")
      import :: c_int, c_ptr
      type(c_ptr), value :: face
      integer(c_int), intent(out) :: cell_width, cell_height
      integer(c_int), intent(out) :: ascender, descender
    end subroutine fortty_ft_get_metrics

    integer(c_int) function fortty_ft_render_glyph(face, codepoint, &
        bitmap, width, height, bearing_x, bearing_y, advance) &
        bind(C, name="fortty_ft_render_glyph")
      import :: c_int, c_ptr
      type(c_ptr), value :: face
      integer(c_int), value :: codepoint
      type(c_ptr), intent(out) :: bitmap
      integer(c_int), intent(out) :: width, height
      integer(c_int), intent(out) :: bearing_x, bearing_y, advance
    end function fortty_ft_render_glyph

    integer(c_int) function fortty_ft_has_glyph(face, codepoint) &
        bind(C, name="fortty_ft_has_glyph")
      import :: c_int, c_ptr
      type(c_ptr), value :: face
      integer(c_int), value :: codepoint
    end function fortty_ft_has_glyph

    ! Fontconfig functions for portable font discovery
    integer(c_int) function fortty_fc_find_font_for_char(codepoint, path, path_size) &
        bind(C, name="fortty_fc_find_font_for_char")
      import :: c_int, c_char
      integer(c_int), value :: codepoint
      character(kind=c_char), intent(out) :: path(*)
      integer(c_int), value :: path_size
    end function fortty_fc_find_font_for_char

    integer(c_int) function fortty_fc_find_monospace_font(path, path_size) &
        bind(C, name="fortty_fc_find_monospace_font")
      import :: c_int, c_char
      character(kind=c_char), intent(out) :: path(*)
      integer(c_int), value :: path_size
    end function fortty_fc_find_monospace_font
  end interface

contains

  ! Load a font from file
  function font_load(path, size_px) result(font)
    character(len=*), intent(in) :: path
    integer, intent(in) :: size_px
    type(font_t) :: font
    character(len=256) :: c_path
    integer(c_int) :: err
    integer(c_int) :: cw, ch, asc, desc

    ! Initialize FreeType
    err = fortty_ft_init(font%ft_library)
    if (err /= 0) then
      print *, "Error: Failed to initialize FreeType, error ", err
      return
    end if

    ! Load font face
    c_path = trim(path) // c_null_char
    err = fortty_ft_load_font(font%ft_library, c_path, int(size_px, c_int), font%ft_face)
    if (err /= 0) then
      print *, "Error: Failed to load font '", trim(path), "', error ", err
      call fortty_ft_done(font%ft_library)
      font%ft_library = c_null_ptr
      return
    end if

    ! Get metrics
    call fortty_ft_get_metrics(font%ft_face, cw, ch, asc, desc)
    font%size_px = size_px
    font%cell_width = cw
    font%cell_height = ch
    font%ascender = asc
    font%descender = desc
    font%loaded = .true.

  end function font_load

  ! Destroy font and free resources
  subroutine font_destroy(font)
    type(font_t), intent(inout) :: font

    if (c_associated(font%ft_face_fallback)) then
      call fortty_ft_done_face(font%ft_face_fallback)
      font%ft_face_fallback = c_null_ptr
    end if

    if (c_associated(font%ft_face)) then
      call fortty_ft_done_face(font%ft_face)
      font%ft_face = c_null_ptr
    end if

    if (c_associated(font%ft_library)) then
      call fortty_ft_done(font%ft_library)
      font%ft_library = c_null_ptr
    end if

    font%loaded = .false.
    font%has_fallback = .false.
  end subroutine font_destroy

  ! Render a glyph and return its data
  ! Returns bitmap pointer (valid until next render_glyph call)
  function font_render_glyph(font, codepoint, bitmap_ptr) result(glyph)
    type(font_t), intent(in) :: font
    integer, intent(in) :: codepoint
    type(c_ptr), intent(out) :: bitmap_ptr
    type(glyph_t) :: glyph
    integer(c_int) :: err, w, h, bx, by, adv

    call glyph_init(glyph)

    if (.not. font%loaded) then
      bitmap_ptr = c_null_ptr
      return
    end if

    err = fortty_ft_render_glyph(font%ft_face, int(codepoint, c_int), &
                                  bitmap_ptr, w, h, bx, by, adv)
    if (err /= 0) then
      bitmap_ptr = c_null_ptr
      return
    end if

    glyph%codepoint = codepoint
    glyph%width = w
    glyph%height = h
    glyph%bearing_x = bx
    glyph%bearing_y = by
    glyph%advance = adv
    glyph%valid = .true.

  end function font_render_glyph

  ! Load a fallback font (for missing glyphs)
  subroutine font_load_fallback(font, path)
    type(font_t), intent(inout) :: font
    character(len=*), intent(in) :: path
    character(len=256) :: c_path
    integer(c_int) :: err
    integer(c_int) :: fb_cell_w, fb_cell_h, fb_asc, fb_desc

    if (.not. font%loaded) return
    if (len_trim(path) == 0) return

    c_path = trim(path) // c_null_char
    err = fortty_ft_load_font(font%ft_library, c_path, int(font%size_px, c_int), &
                               font%ft_face_fallback)
    if (err /= 0) then
      ! Silently fail - fallback is optional
      font%ft_face_fallback = c_null_ptr
      font%has_fallback = .false.
      return
    end if

    font%has_fallback = .true.

    ! Get fallback font metrics for baseline alignment
    call fortty_ft_get_metrics(font%ft_face_fallback, fb_cell_w, fb_cell_h, fb_asc, fb_desc)
    font%fallback_ascender = fb_asc
  end subroutine font_load_fallback

  ! Check if a glyph exists in the primary font
  function font_has_glyph(font, codepoint) result(has)
    type(font_t), intent(in) :: font
    integer, intent(in) :: codepoint
    logical :: has

    has = .false.
    if (.not. font%loaded) return

    has = fortty_ft_has_glyph(font%ft_face, int(codepoint, c_int)) /= 0
  end function font_has_glyph

  ! Render glyph with fallback support
  ! used_fallback is set to .true. if glyph came from fallback font
  function font_render_glyph_with_fallback(font, codepoint, bitmap_ptr, used_fallback) result(glyph)
    type(font_t), intent(in) :: font
    integer, intent(in) :: codepoint
    type(c_ptr), intent(out) :: bitmap_ptr
    logical, intent(out) :: used_fallback
    type(glyph_t) :: glyph
    integer(c_int) :: err, w, h, bx, by, adv
    type(c_ptr) :: face_to_use
    integer(c_int) :: primary_has, fallback_has

    call glyph_init(glyph)
    used_fallback = .false.

    if (.not. font%loaded) then
      bitmap_ptr = c_null_ptr
      return
    end if

    ! Check if primary font has this glyph
    primary_has = fortty_ft_has_glyph(font%ft_face, int(codepoint, c_int))
    if (primary_has /= 0) then
      face_to_use = font%ft_face
    else if (font%has_fallback .and. c_associated(font%ft_face_fallback)) then
      ! Try fallback font
      fallback_has = fortty_ft_has_glyph(font%ft_face_fallback, int(codepoint, c_int))
      if (fallback_has /= 0) then
        face_to_use = font%ft_face_fallback
        used_fallback = .true.
      else
        ! Neither font has this glyph
        bitmap_ptr = c_null_ptr
        return
      end if
    else
      ! No fallback available, try primary anyway (may show .notdef)
      face_to_use = font%ft_face
    end if

    err = fortty_ft_render_glyph(face_to_use, int(codepoint, c_int), &
                                  bitmap_ptr, w, h, bx, by, adv)
    if (err /= 0) then
      bitmap_ptr = c_null_ptr
      return
    end if

    glyph%codepoint = codepoint
    glyph%width = w
    glyph%height = h
    glyph%bearing_x = bx
    glyph%bearing_y = by
    glyph%advance = adv
    glyph%valid = .true.

    ! Normalize baseline: adjust bearing_y for ascender difference between fonts
    if (used_fallback) then
      glyph%bearing_y = glyph%bearing_y - (font%fallback_ascender - font%ascender)
    end if

  end function font_render_glyph_with_fallback

  ! Find a font that supports a specific codepoint using fontconfig
  ! Returns empty string if fontconfig not available or no font found
  function font_find_for_codepoint(codepoint) result(path)
    integer, intent(in) :: codepoint
    character(len=256) :: path
    character(len=256) :: c_path
    integer(c_int) :: err
    integer :: i

    path = ''
    c_path = ''
    err = fortty_fc_find_font_for_char(int(codepoint, c_int), c_path, 256_c_int)
    if (err == 0) then
      ! Convert C string to Fortran string
      do i = 1, 256
        if (c_path(i:i) == c_null_char) exit
        path(i:i) = c_path(i:i)
      end do
    end if
  end function font_find_for_codepoint

  ! Find a monospace font using fontconfig
  ! Returns empty string if fontconfig not available or no font found
  function font_find_monospace() result(path)
    character(len=256) :: path
    character(len=256) :: c_path
    integer(c_int) :: err
    integer :: i

    path = ''
    c_path = ''
    err = fortty_fc_find_monospace_font(c_path, 256_c_int)
    if (err == 0) then
      ! Convert C string to Fortran string
      do i = 1, 256
        if (c_path(i:i) == c_null_char) exit
        path(i:i) = c_path(i:i)
      end do
    end if
  end function font_find_monospace

end module font_mod
