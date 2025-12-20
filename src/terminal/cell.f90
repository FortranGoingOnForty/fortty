module cell_mod
  implicit none
  private

  public :: color_t, cell_t
  public :: COLOR_DEFAULT, COLOR_INDEXED, COLOR_RGB
  public :: ATTR_BOLD, ATTR_ITALIC, ATTR_UNDERLINE, ATTR_BLINK
  public :: ATTR_INVERSE, ATTR_HIDDEN, ATTR_STRIKETHROUGH
  public :: default_fg, default_bg, make_cell
  public :: color_from_index, color_from_rgb
  public :: set_palette_color, set_default_colors

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

  ! Default colors (can be overridden by config)
  type(color_t) :: default_fg = color_t(COLOR_DEFAULT, 7, 255, 255, 255)
  type(color_t) :: default_bg = color_t(COLOR_DEFAULT, 0, 0, 0, 0)

  ! Configurable 16-color palette (indexed 0-15)
  ! Can be overridden via set_palette_color
  type(color_t), save :: custom_palette(0:15)
  logical, save :: palette_initialized = .false.

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

  ! Initialize custom palette with default VGA colors
  subroutine init_default_palette()
    integer :: i
    ! VGA palette RGB values
    integer, parameter :: vga_r(0:15) = [0, 128, 0, 128, 0, 128, 0, 192, 128, 255, 0, 255, 0, 255, 0, 255]
    integer, parameter :: vga_g(0:15) = [0, 0, 128, 128, 0, 0, 128, 192, 128, 0, 255, 255, 0, 0, 255, 255]
    integer, parameter :: vga_b(0:15) = [0, 0, 0, 0, 128, 128, 128, 192, 128, 0, 0, 0, 255, 255, 255, 255]

    do i = 0, 15
      custom_palette(i) = color_t(COLOR_RGB, i, vga_r(i), vga_g(i), vga_b(i))
    end do
    palette_initialized = .true.
  end subroutine init_default_palette

  ! Set a single palette color (index 0-15)
  subroutine set_palette_color(idx, c)
    integer, intent(in) :: idx
    type(color_t), intent(in) :: c

    if (.not. palette_initialized) call init_default_palette()
    if (idx >= 0 .and. idx <= 15) then
      custom_palette(idx) = c
    end if
  end subroutine set_palette_color

  ! Set default foreground and background colors
  subroutine set_default_colors(fg, bg)
    type(color_t), intent(in) :: fg, bg

    default_fg = fg
    default_bg = bg
  end subroutine set_default_colors

  ! Create color from 256-color palette index
  function color_from_index(idx) result(c)
    integer, intent(in) :: idx
    type(color_t) :: c
    integer :: r, g, b, gray

    ! Initialize palette on first use if needed
    if (.not. palette_initialized) call init_default_palette()

    c%mode = COLOR_RGB
    c%index = idx

    if (idx < 0 .or. idx > 255) then
      ! Invalid index - return white
      c%r = 255; c%g = 255; c%b = 255
    else if (idx < 16) then
      ! Use custom palette for standard 16 colors
      c = custom_palette(idx)
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
