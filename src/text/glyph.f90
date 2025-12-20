module glyph_mod
  use, intrinsic :: iso_c_binding
  implicit none
  public

  ! Glyph information for a single character
  type :: glyph_t
    integer :: codepoint = 0         ! Unicode codepoint
    integer :: tex_x = 0, tex_y = 0  ! Position in texture atlas
    integer :: width = 0, height = 0 ! Bitmap dimensions
    integer :: bearing_x = 0         ! Offset from cursor X to left edge
    integer :: bearing_y = 0         ! Offset from baseline to top edge
    integer :: advance = 0           ! Horizontal advance to next character
    real(c_float) :: u0 = 0.0, v0 = 0.0  ! UV coords (top-left)
    real(c_float) :: u1 = 0.0, v1 = 0.0  ! UV coords (bottom-right)
    logical :: valid = .false.       ! Whether glyph data is loaded
  end type glyph_t

contains

  ! Initialize a glyph with default values
  subroutine glyph_init(g)
    type(glyph_t), intent(out) :: g
    g%codepoint = 0
    g%tex_x = 0
    g%tex_y = 0
    g%width = 0
    g%height = 0
    g%bearing_x = 0
    g%bearing_y = 0
    g%advance = 0
    g%u0 = 0.0
    g%v0 = 0.0
    g%u1 = 0.0
    g%v1 = 0.0
    g%valid = .false.
  end subroutine glyph_init

end module glyph_mod
