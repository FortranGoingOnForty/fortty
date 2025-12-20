module font_mod
  use, intrinsic :: iso_c_binding
  use glyph_mod
  implicit none
  private

  public :: font_t
  public :: font_load, font_destroy, font_render_glyph

  ! Font handle and metrics
  type :: font_t
    type(c_ptr) :: ft_library = c_null_ptr
    type(c_ptr) :: ft_face = c_null_ptr
    integer :: size_px = 0
    integer :: cell_width = 0
    integer :: cell_height = 0
    integer :: ascender = 0
    integer :: descender = 0
    logical :: loaded = .false.
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

    if (c_associated(font%ft_face)) then
      call fortty_ft_done_face(font%ft_face)
      font%ft_face = c_null_ptr
    end if

    if (c_associated(font%ft_library)) then
      call fortty_ft_done(font%ft_library)
      font%ft_library = c_null_ptr
    end if

    font%loaded = .false.
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

end module font_mod
