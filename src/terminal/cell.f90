module cell_mod
  implicit none
  private

  public :: color_t, cell_t
  public :: COLOR_DEFAULT, COLOR_INDEXED, COLOR_RGB
  public :: ATTR_BOLD, ATTR_ITALIC, ATTR_UNDERLINE, ATTR_BLINK
  public :: ATTR_INVERSE, ATTR_HIDDEN, ATTR_STRIKETHROUGH
  public :: default_fg, default_bg, make_cell
  public :: color_from_index, color_from_rgb

  ! Color modes
  integer, parameter :: COLOR_DEFAULT = 0
  integer, parameter :: COLOR_INDEXED = 1
  integer, parameter :: COLOR_RGB = 2

  ! Attribute flags (can be combined with IOR)
  integer, parameter :: ATTR_BOLD          = 1
  integer, parameter :: ATTR_ITALIC        = 2
  integer, parameter :: ATTR_UNDERLINE     = 4
  integer, parameter :: ATTR_BLINK         = 8
  integer, parameter :: ATTR_INVERSE       = 16
  integer, parameter :: ATTR_HIDDEN        = 32
  integer, parameter :: ATTR_STRIKETHROUGH = 64

  type :: color_t
    integer :: mode = COLOR_DEFAULT
    integer :: index = 7              ! Palette index (0-255)
    integer :: r = 255, g = 255, b = 255  ! RGB values
  end type color_t

  type :: cell_t
    integer :: codepoint = 32         ! Unicode codepoint (32 = space)
    type(color_t) :: fg               ! Foreground color
    type(color_t) :: bg               ! Background color
    integer :: attrs = 0              ! Attribute flags
  end type cell_t

  ! Default colors
  type(color_t), parameter :: default_fg = color_t(COLOR_DEFAULT, 7, 255, 255, 255)
  type(color_t), parameter :: default_bg = color_t(COLOR_DEFAULT, 0, 0, 0, 0)

contains

  ! Create a cell with given codepoint and current style
  function make_cell(codepoint, fg, bg, attrs) result(c)
    integer, intent(in) :: codepoint
    type(color_t), intent(in) :: fg, bg
    integer, intent(in) :: attrs
    type(cell_t) :: c

    c%codepoint = codepoint
    c%fg = fg
    c%bg = bg
    c%attrs = attrs
  end function make_cell

  ! Create color from 256-color palette index
  function color_from_index(idx) result(c)
    integer, intent(in) :: idx
    type(color_t) :: c
    integer :: r, g, b, gray

    ! Standard 16 colors (VGA palette)
    integer, parameter :: palette(0:15, 3) = reshape([ &
      0,   0,   0,   &  ! 0  Black
      128, 0,   0,   &  ! 1  Red
      0,   128, 0,   &  ! 2  Green
      128, 128, 0,   &  ! 3  Yellow
      0,   0,   128, &  ! 4  Blue
      128, 0,   128, &  ! 5  Magenta
      0,   128, 128, &  ! 6  Cyan
      192, 192, 192, &  ! 7  White
      128, 128, 128, &  ! 8  Bright Black (Gray)
      255, 0,   0,   &  ! 9  Bright Red
      0,   255, 0,   &  ! 10 Bright Green
      255, 255, 0,   &  ! 11 Bright Yellow
      0,   0,   255, &  ! 12 Bright Blue
      255, 0,   255, &  ! 13 Bright Magenta
      0,   255, 255, &  ! 14 Bright Cyan
      255, 255, 255  &  ! 15 Bright White
    ], [16, 3])

    c%mode = COLOR_RGB
    c%index = idx

    if (idx < 0 .or. idx > 255) then
      ! Invalid index - return white
      c%r = 255; c%g = 255; c%b = 255
    else if (idx < 16) then
      ! Standard 16 colors
      c%r = palette(idx, 1)
      c%g = palette(idx, 2)
      c%b = palette(idx, 3)
    else if (idx < 232) then
      ! 6x6x6 color cube (indices 16-231)
      ! index = 16 + 36*r + 6*g + b where r,g,b in 0-5
      r = (idx - 16) / 36
      g = mod((idx - 16) / 6, 6)
      b = mod(idx - 16, 6)
      ! Map 0-5 to 0,95,135,175,215,255
      if (r == 0) then; c%r = 0; else; c%r = 55 + r * 40; end if
      if (g == 0) then; c%g = 0; else; c%g = 55 + g * 40; end if
      if (b == 0) then; c%b = 0; else; c%b = 55 + b * 40; end if
    else
      ! Grayscale ramp (indices 232-255)
      ! 24 shades from 8 to 238
      gray = 8 + (idx - 232) * 10
      c%r = gray; c%g = gray; c%b = gray
    end if
  end function color_from_index

  ! Create color from RGB values
  function color_from_rgb(r, g, b) result(c)
    integer, intent(in) :: r, g, b
    type(color_t) :: c

    c%mode = COLOR_RGB
    c%index = 0
    c%r = max(0, min(255, r))
    c%g = max(0, min(255, g))
    c%b = max(0, min(255, b))
  end function color_from_rgb

end module cell_mod
