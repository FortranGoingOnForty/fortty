module cell_mod
  implicit none
  private

  public :: color_t, cell_t
  public :: COLOR_DEFAULT, COLOR_INDEXED, COLOR_RGB
  public :: ATTR_BOLD, ATTR_ITALIC, ATTR_UNDERLINE, ATTR_BLINK
  public :: ATTR_INVERSE, ATTR_HIDDEN, ATTR_STRIKETHROUGH
  public :: default_fg, default_bg, make_cell

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

end module cell_mod
