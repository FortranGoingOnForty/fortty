module config_mod
  use cell_mod, only: color_t, COLOR_RGB
  use toml_parser_mod
  implicit none
  private

  public :: config_t
  public :: config_load, config_get_path, config_init_defaults

  type :: config_t
    ! Window
    integer :: window_width = 800
    integer :: window_height = 600
    real :: window_opacity = 1.0      ! 0.0 (transparent) to 1.0 (opaque)
    logical :: window_blur = .false.  ! macOS only: enable background blur

    ! Font
    character(len=256) :: font_path = ''       ! Empty = use fontconfig
    character(len=256) :: font_fallback = ''   ! Empty = auto-detect
    integer :: font_size = 16

    ! Colors
    type(color_t) :: fg_color
    type(color_t) :: bg_color
    type(color_t) :: cursor_color
    type(color_t) :: palette(0:15)

    ! Terminal
    integer :: scrollback_lines = 10000

    ! Shell
    character(len=256) :: shell_program = ''

    ! State
    logical :: loaded = .false.
  end type config_t

contains

  ! Initialize config with default values
  subroutine config_init_defaults(cfg)
    type(config_t), intent(inout) :: cfg

    cfg%window_width = 800
    cfg%window_height = 600
    cfg%window_opacity = 1.0
    cfg%window_blur = .false.

    cfg%font_path = ''
    cfg%font_fallback = ''
    cfg%font_size = 16

    ! Default colors (white on dark gray)
    cfg%fg_color = color_t(COLOR_RGB, 7, 255, 255, 255)
    cfg%bg_color = color_t(COLOR_RGB, 0, 26, 26, 30)   ! #1A1A1E
    cfg%cursor_color = color_t(COLOR_RGB, 7, 179, 179, 179)  ! #B3B3B3

    ! Default 16-color palette (VGA-ish)
    cfg%palette(0)  = make_rgb_color(0, 0, 0)         ! Black
    cfg%palette(1)  = make_rgb_color(128, 0, 0)       ! Red
    cfg%palette(2)  = make_rgb_color(0, 128, 0)       ! Green
    cfg%palette(3)  = make_rgb_color(128, 128, 0)     ! Yellow
    cfg%palette(4)  = make_rgb_color(0, 0, 128)       ! Blue
    cfg%palette(5)  = make_rgb_color(128, 0, 128)     ! Magenta
    cfg%palette(6)  = make_rgb_color(0, 128, 128)     ! Cyan
    cfg%palette(7)  = make_rgb_color(192, 192, 192)   ! White
    cfg%palette(8)  = make_rgb_color(128, 128, 128)   ! Bright Black
    cfg%palette(9)  = make_rgb_color(255, 0, 0)       ! Bright Red
    cfg%palette(10) = make_rgb_color(0, 255, 0)       ! Bright Green
    cfg%palette(11) = make_rgb_color(255, 255, 0)     ! Bright Yellow
    cfg%palette(12) = make_rgb_color(0, 0, 255)       ! Bright Blue
    cfg%palette(13) = make_rgb_color(255, 0, 255)     ! Bright Magenta
    cfg%palette(14) = make_rgb_color(0, 255, 255)     ! Bright Cyan
    cfg%palette(15) = make_rgb_color(255, 255, 255)   ! Bright White

    cfg%scrollback_lines = 10000
    cfg%shell_program = ''
    cfg%loaded = .false.

  end subroutine config_init_defaults

  ! Get the path to the config file (XDG or ~/.config)
  function config_get_path() result(path)
    character(len=512) :: path
    character(len=256) :: xdg_config, home
    integer :: len_val
    logical :: file_exists

    path = ''

    ! Try XDG_CONFIG_HOME first
    call get_environment_variable('XDG_CONFIG_HOME', xdg_config, len_val)
    if (len_val > 0) then
      path = trim(xdg_config) // '/fortty/fortty.toml'
      inquire(file=path, exist=file_exists)
      if (file_exists) return
    end if

    ! Fall back to ~/.config
    call get_environment_variable('HOME', home, len_val)
    if (len_val > 0) then
      path = trim(home) // '/.config/fortty/fortty.toml'
      inquire(file=path, exist=file_exists)
      if (file_exists) return
    end if

    ! No config file found
    path = ''

  end function config_get_path

  ! Load configuration from file
  function config_load(path) result(cfg)
    character(len=*), intent(in) :: path
    type(config_t) :: cfg
    type(toml_file_t) :: tf
    character(len=512) :: config_path
    character(len=256) :: str_val

    ! Initialize with defaults
    call config_init_defaults(cfg)

    ! If no path provided, try to find config file
    if (len_trim(path) == 0) then
      config_path = config_get_path()
      if (len_trim(config_path) == 0) then
        ! No config file found, use defaults
        return
      end if
      tf = toml_open(trim(config_path))
    else
      tf = toml_open(path)
    end if

    if (.not. tf%loaded) return

    ! Window settings
    cfg%window_width = toml_get_integer(tf, 'window', 'width', cfg%window_width)
    cfg%window_height = toml_get_integer(tf, 'window', 'height', cfg%window_height)
    cfg%window_opacity = toml_get_real(tf, 'window', 'opacity', cfg%window_opacity)
    cfg%window_blur = toml_get_logical(tf, 'window', 'blur', cfg%window_blur)

    ! Font settings
    str_val = toml_get_string(tf, 'font', 'family', '')
    if (len_trim(str_val) > 0) cfg%font_path = str_val
    str_val = toml_get_string(tf, 'font', 'fallback', '')
    if (len_trim(str_val) > 0) cfg%font_fallback = str_val
    cfg%font_size = toml_get_integer(tf, 'font', 'size', cfg%font_size)

    ! Color settings
    str_val = toml_get_string(tf, 'colors', 'foreground', '')
    if (len_trim(str_val) > 0) cfg%fg_color = parse_hex_color(str_val)
    str_val = toml_get_string(tf, 'colors', 'background', '')
    if (len_trim(str_val) > 0) cfg%bg_color = parse_hex_color(str_val)
    str_val = toml_get_string(tf, 'colors', 'cursor', '')
    if (len_trim(str_val) > 0) cfg%cursor_color = parse_hex_color(str_val)

    ! Palette colors
    call load_palette_color(tf, 'black', cfg%palette(0))
    call load_palette_color(tf, 'red', cfg%palette(1))
    call load_palette_color(tf, 'green', cfg%palette(2))
    call load_palette_color(tf, 'yellow', cfg%palette(3))
    call load_palette_color(tf, 'blue', cfg%palette(4))
    call load_palette_color(tf, 'magenta', cfg%palette(5))
    call load_palette_color(tf, 'cyan', cfg%palette(6))
    call load_palette_color(tf, 'white', cfg%palette(7))
    call load_palette_color(tf, 'bright_black', cfg%palette(8))
    call load_palette_color(tf, 'bright_red', cfg%palette(9))
    call load_palette_color(tf, 'bright_green', cfg%palette(10))
    call load_palette_color(tf, 'bright_yellow', cfg%palette(11))
    call load_palette_color(tf, 'bright_blue', cfg%palette(12))
    call load_palette_color(tf, 'bright_magenta', cfg%palette(13))
    call load_palette_color(tf, 'bright_cyan', cfg%palette(14))
    call load_palette_color(tf, 'bright_white', cfg%palette(15))

    ! Terminal settings
    cfg%scrollback_lines = toml_get_integer(tf, 'terminal', 'scrollback_lines', cfg%scrollback_lines)

    ! Shell settings
    str_val = toml_get_string(tf, 'shell', 'program', '')
    if (len_trim(str_val) > 0) cfg%shell_program = str_val

    call toml_close(tf)
    cfg%loaded = .true.

  end function config_load

  ! Helper: Load a palette color from TOML
  subroutine load_palette_color(tf, key, color)
    type(toml_file_t), intent(in) :: tf
    character(len=*), intent(in) :: key
    type(color_t), intent(inout) :: color
    character(len=256) :: str_val

    str_val = toml_get_string(tf, 'colors', key, '')
    if (len_trim(str_val) > 0) then
      color = parse_hex_color(str_val)
    end if
  end subroutine load_palette_color

  ! Parse a hex color string (#RRGGBB) to color_t
  function parse_hex_color(hex_str) result(c)
    character(len=*), intent(in) :: hex_str
    type(color_t) :: c
    character(len=2) :: r_str, g_str, b_str
    integer :: r, g, b, start_pos

    c = color_t(COLOR_RGB, 0, 255, 255, 255)  ! Default white

    ! Skip leading # if present
    if (hex_str(1:1) == '#') then
      start_pos = 2
    else
      start_pos = 1
    end if

    ! Need at least 6 characters for RRGGBB
    if (len_trim(hex_str) < start_pos + 5) return

    r_str = hex_str(start_pos:start_pos+1)
    g_str = hex_str(start_pos+2:start_pos+3)
    b_str = hex_str(start_pos+4:start_pos+5)

    r = hex_to_int(r_str)
    g = hex_to_int(g_str)
    b = hex_to_int(b_str)

    c = make_rgb_color(r, g, b)

  end function parse_hex_color

  ! Convert 2-character hex string to integer
  function hex_to_int(hex_str) result(val)
    character(len=2), intent(in) :: hex_str
    integer :: val
    integer :: i, digit
    character(len=1) :: ch

    val = 0
    do i = 1, 2
      ch = hex_str(i:i)
      if (ch >= '0' .and. ch <= '9') then
        digit = ichar(ch) - ichar('0')
      else if (ch >= 'a' .and. ch <= 'f') then
        digit = ichar(ch) - ichar('a') + 10
      else if (ch >= 'A' .and. ch <= 'F') then
        digit = ichar(ch) - ichar('A') + 10
      else
        digit = 0
      end if
      val = val * 16 + digit
    end do

  end function hex_to_int

  ! Helper: Create an RGB color
  function make_rgb_color(r, g, b) result(c)
    integer, intent(in) :: r, g, b
    type(color_t) :: c

    c%mode = COLOR_RGB
    c%index = 0
    c%r = r
    c%g = g
    c%b = b
  end function make_rgb_color

end module config_mod
