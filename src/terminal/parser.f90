module parser_mod
  use cell_mod
  use screen_mod
  use terminal_mod
  use cursor_mod, only: CURSOR_BLOCK, CURSOR_UNDERLINE, CURSOR_BAR
  implicit none
  private

  public :: parser_t
  public :: parser_init, parser_process_byte

  ! Parser states
  integer, parameter :: STATE_GROUND = 0
  integer, parameter :: STATE_ESCAPE = 1
  integer, parameter :: STATE_CSI_ENTRY = 2
  integer, parameter :: STATE_CSI_PARAM = 3
  integer, parameter :: STATE_OSC_STRING = 4

  integer, parameter :: MAX_PARAMS = 16

  type :: parser_t
    integer :: state = STATE_GROUND

    ! CSI parameter accumulation
    integer :: params(MAX_PARAMS)
    integer :: param_count = 0
    integer :: current_param = 0
    logical :: param_started = .false.

    ! For CSI ? and > sequences
    logical :: has_private = .false.
    character(len=1) :: private_marker = ' '

    ! Intermediate character (for sequences like CSI Ps SP q)
    character(len=1) :: intermediate = ' '
    logical :: has_intermediate = .false.

    ! OSC string buffer (for window title, etc.)
    character(len=256) :: osc_buffer
    integer :: osc_len = 0

    ! UTF-8 decoding state
    integer :: utf8_codepoint = 0
    integer :: utf8_remaining = 0
  end type parser_t

contains

  ! Initialize parser
  subroutine parser_init(p)
    type(parser_t), intent(inout) :: p

    call parser_reset(p)
    p%utf8_codepoint = 0
    p%utf8_remaining = 0
  end subroutine parser_init

  ! Reset parser to ground state
  subroutine parser_reset(p)
    type(parser_t), intent(inout) :: p

    p%state = STATE_GROUND
    p%params = 0
    p%param_count = 0
    p%current_param = 0
    p%param_started = .false.
    p%has_private = .false.
    p%private_marker = ' '
    p%has_intermediate = .false.
    p%intermediate = ' '
    p%osc_buffer = ''
    p%osc_len = 0
    ! Note: Don't reset UTF-8 state here - it persists across escape sequences
  end subroutine parser_reset

  ! Reset UTF-8 decoding state (call on invalid sequences)
  subroutine parser_reset_utf8(p)
    type(parser_t), intent(inout) :: p

    p%utf8_codepoint = 0
    p%utf8_remaining = 0
  end subroutine parser_reset_utf8

  ! Process a single byte from PTY output
  subroutine parser_process_byte(p, term, byte)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: byte

    select case (p%state)
      case (STATE_GROUND)
        call handle_ground(p, term, byte)
      case (STATE_ESCAPE)
        call handle_escape(p, term, byte)
      case (STATE_CSI_ENTRY, STATE_CSI_PARAM)
        call handle_csi(p, term, byte)
      case (STATE_OSC_STRING)
        call handle_osc(p, term, byte)
    end select
  end subroutine parser_process_byte

  ! Handle ground state - normal character processing with UTF-8 decoding
  recursive subroutine handle_ground(p, term, byte)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: byte

    ! Check for escape sequence start
    if (byte == 27) then  ! ESC
      ! Abort any in-progress UTF-8 sequence
      call parser_reset_utf8(p)
      p%state = STATE_ESCAPE
      return
    end if

    ! UTF-8 decoding state machine
    if (p%utf8_remaining > 0) then
      ! Expecting continuation byte (10xxxxxx)
      if (iand(byte, 192) == 128) then  ! 192 = 0xC0, 128 = 0x80
        ! Valid continuation byte
        p%utf8_codepoint = ior(ishft(p%utf8_codepoint, 6), iand(byte, 63))
        p%utf8_remaining = p%utf8_remaining - 1

        if (p%utf8_remaining == 0) then
          ! UTF-8 sequence complete - output the codepoint
          call terminal_put_char(term, p%utf8_codepoint)
          p%utf8_codepoint = 0
        end if
      else
        ! Invalid continuation - reset and process byte as new start
        call parser_reset_utf8(p)
        call handle_ground(p, term, byte)
      end if
      return
    end if

    ! Check for UTF-8 lead byte
    if (byte < 128) then
      ! ASCII (0xxxxxxx) - single byte, pass directly
      call terminal_put_char(term, byte)

    else if (iand(byte, 224) == 192) then  ! 224 = 0xE0, 192 = 0xC0
      ! 2-byte sequence (110xxxxx)
      p%utf8_codepoint = iand(byte, 31)  ! Keep lower 5 bits
      p%utf8_remaining = 1

    else if (iand(byte, 240) == 224) then  ! 240 = 0xF0, 224 = 0xE0
      ! 3-byte sequence (1110xxxx)
      p%utf8_codepoint = iand(byte, 15)  ! Keep lower 4 bits
      p%utf8_remaining = 2

    else if (iand(byte, 248) == 240) then  ! 248 = 0xF8, 240 = 0xF0
      ! 4-byte sequence (11110xxx)
      p%utf8_codepoint = iand(byte, 7)   ! Keep lower 3 bits
      p%utf8_remaining = 3

    else
      ! Invalid UTF-8 lead byte (continuation byte without lead, or invalid)
      ! Pass through as-is (shows replacement character behavior)
      call terminal_put_char(term, byte)
    end if

  end subroutine handle_ground

  ! Handle escape state - ESC received
  subroutine handle_escape(p, term, byte)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: byte

    select case (byte)
      case (91)  ! '[' - CSI introducer
        p%state = STATE_CSI_ENTRY
        p%params = 0
        p%param_count = 0
        p%current_param = 0
        p%param_started = .false.
        p%has_private = .false.
        p%private_marker = ' '

      case (93)  ! ']' - OSC introducer
        p%state = STATE_OSC_STRING
        p%osc_buffer = ''
        p%osc_len = 0

      case (55)  ! '7' - DECSC (save cursor)
        call terminal_save_cursor(term)
        p%state = STATE_GROUND

      case (56)  ! '8' - DECRC (restore cursor)
        call terminal_restore_cursor(term)
        p%state = STATE_GROUND

      case (68)  ! 'D' - IND (index/linefeed)
        call terminal_index(term)
        p%state = STATE_GROUND

      case (69)  ! 'E' - NEL (next line)
        call terminal_carriage_return(term)
        call terminal_index(term)
        p%state = STATE_GROUND

      case (77)  ! 'M' - RI (reverse index)
        call terminal_reverse_index(term)
        p%state = STATE_GROUND

      case (99)  ! 'c' - RIS (reset to initial state)
        call terminal_reset(term)
        p%state = STATE_GROUND

      case default
        ! Unknown escape sequence - return to ground
        p%state = STATE_GROUND
    end select
  end subroutine handle_escape

  ! Handle CSI state - collecting parameters
  subroutine handle_csi(p, term, byte)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: byte

    ! Check for private marker (?, >, etc.)
    if (p%state == STATE_CSI_ENTRY) then
      if (byte == 63 .or. byte == 62 .or. byte == 60 .or. byte == 61) then
        ! '?' = 63, '>' = 62, '<' = 60, '=' = 61
        p%has_private = .true.
        p%private_marker = char(byte)
        p%state = STATE_CSI_PARAM
        return
      end if
      p%state = STATE_CSI_PARAM
    end if

    ! Numeric parameter
    if (byte >= 48 .and. byte <= 57) then  ! '0'-'9'
      p%current_param = p%current_param * 10 + (byte - 48)
      p%param_started = .true.
      return
    end if

    ! Parameter separator
    if (byte == 59) then  ! ';'
      if (p%param_count < MAX_PARAMS) then
        p%param_count = p%param_count + 1
        p%params(p%param_count) = p%current_param
      end if
      p%current_param = 0
      p%param_started = .false.
      return
    end if

    ! Final byte - dispatch the command
    if (byte >= 64 .and. byte <= 126) then  ! '@'-'~'
      ! Store last parameter
      if (p%param_started .or. p%param_count > 0) then
        if (p%param_count < MAX_PARAMS) then
          p%param_count = p%param_count + 1
          p%params(p%param_count) = p%current_param
        end if
      end if

      call dispatch_csi(p, term, byte)
      call parser_reset(p)
      return
    end if

    ! Intermediate byte (space, !, ", etc.) - store it
    if (byte >= 32 .and. byte <= 47) then
      p%has_intermediate = .true.
      p%intermediate = char(byte)
      return
    end if

    ! Unexpected byte - abort sequence
    call parser_reset(p)
  end subroutine handle_csi

  ! Handle OSC state - operating system command
  subroutine handle_osc(p, term, byte)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: byte

    ! OSC ends with BEL (7) or ST (ESC \)
    if (byte == 7) then  ! BEL
      call dispatch_osc(p, term)
      call parser_reset(p)
      return
    end if

    if (byte == 27) then  ! ESC - might be ST
      ! For simplicity, dispatch and reset
      call dispatch_osc(p, term)
      call parser_reset(p)
      return
    end if

    ! Accumulate OSC string
    if (p%osc_len < 256) then
      p%osc_len = p%osc_len + 1
      p%osc_buffer(p%osc_len:p%osc_len) = char(byte)
    end if
  end subroutine handle_osc

  ! Dispatch OSC command
  subroutine dispatch_osc(p, term)
    type(parser_t), intent(in) :: p
    type(terminal_t), intent(inout) :: term
    integer :: cmd, sep_pos, i
    character(len=256) :: title_text

    if (p%osc_len == 0) return

    ! Parse command number (before first ';')
    sep_pos = 0
    do i = 1, p%osc_len
      if (p%osc_buffer(i:i) == ';') then
        sep_pos = i
        exit
      end if
    end do

    if (sep_pos == 0) return  ! No separator found

    ! Extract command number
    cmd = 0
    do i = 1, sep_pos - 1
      if (p%osc_buffer(i:i) >= '0' .and. p%osc_buffer(i:i) <= '9') then
        cmd = cmd * 10 + (ichar(p%osc_buffer(i:i)) - ichar('0'))
      end if
    end do

    ! Extract title text (after ';')
    title_text = ''
    if (sep_pos < p%osc_len) then
      title_text = p%osc_buffer(sep_pos + 1:p%osc_len)
    end if

    select case (cmd)
      case (0)  ! Set icon name and window title
        call terminal_set_title(term, trim(title_text))
      case (1)  ! Set icon name only (we treat as title too)
        call terminal_set_title(term, trim(title_text))
      case (2)  ! Set window title only
        call terminal_set_title(term, trim(title_text))
      ! Other OSC commands can be added here
    end select
  end subroutine dispatch_osc

  ! Dispatch CSI command based on final byte
  subroutine dispatch_csi(p, term, cmd)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: cmd
    integer :: n, m

    ! Get first two params with defaults
    n = 1
    m = 1
    if (p%param_count >= 1 .and. p%params(1) > 0) n = p%params(1)
    if (p%param_count >= 2 .and. p%params(2) > 0) m = p%params(2)

    ! Handle private sequences (CSI ? ...)
    if (p%has_private .and. p%private_marker == '?') then
      call dispatch_dec_private(p, term, cmd, n)
      return
    end if

    ! Handle DECSCUSR (CSI Ps SP q) - Set Cursor Style
    if (p%has_intermediate .and. p%intermediate == ' ' .and. cmd == 113) then
      ! Ps=0,1: blinking block, Ps=2: steady block
      ! Ps=3: blinking underline, Ps=4: steady underline
      ! Ps=5: blinking bar, Ps=6: steady bar
      select case (n)
        case (0, 1)  ! Blinking block
          term%cursor%style = CURSOR_BLOCK
          term%cursor%blink = .true.
        case (2)  ! Steady block
          term%cursor%style = CURSOR_BLOCK
          term%cursor%blink = .false.
        case (3)  ! Blinking underline
          term%cursor%style = CURSOR_UNDERLINE
          term%cursor%blink = .true.
        case (4)  ! Steady underline
          term%cursor%style = CURSOR_UNDERLINE
          term%cursor%blink = .false.
        case (5)  ! Blinking bar
          term%cursor%style = CURSOR_BAR
          term%cursor%blink = .true.
        case (6)  ! Steady bar
          term%cursor%style = CURSOR_BAR
          term%cursor%blink = .false.
      end select
      return
    end if

    select case (cmd)
      case (65)  ! 'A' - CUU (cursor up)
        call terminal_cursor_up(term, n)

      case (66)  ! 'B' - CUD (cursor down)
        call terminal_cursor_down(term, n)

      case (67)  ! 'C' - CUF (cursor forward)
        call terminal_cursor_forward(term, n)

      case (68)  ! 'D' - CUB (cursor backward)
        call terminal_cursor_backward(term, n)

      case (69)  ! 'E' - CNL (cursor next line)
        call terminal_cursor_down(term, n)
        term%cursor%col = 1

      case (70)  ! 'F' - CPL (cursor previous line)
        call terminal_cursor_up(term, n)
        term%cursor%col = 1

      case (71)  ! 'G' - CHA (cursor horizontal absolute)
        call terminal_cursor_move(term, term%cursor%row, n)

      case (72, 102)  ! 'H' or 'f' - CUP (cursor position)
        call terminal_cursor_move(term, n, m)

      case (74)  ! 'J' - ED (erase in display)
        if (p%param_count == 0) n = 0
        call terminal_erase_display(term, n)

      case (75)  ! 'K' - EL (erase in line)
        if (p%param_count == 0) n = 0
        call terminal_erase_line(term, n)

      case (76)  ! 'L' - IL (insert lines)
        call terminal_insert_lines(term, n)

      case (77)  ! 'M' - DL (delete lines)
        call terminal_delete_lines(term, n)

      case (80)  ! 'P' - DCH (delete characters)
        call terminal_delete_chars(term, n)

      case (64)  ! '@' - ICH (insert characters)
        call terminal_insert_chars(term, n)

      case (88)  ! 'X' - ECH (erase characters)
        call terminal_erase_chars(term, n)

      case (100)  ! 'd' - VPA (vertical line position absolute)
        call terminal_cursor_move(term, n, term%cursor%col)

      case (109)  ! 'm' - SGR (select graphic rendition)
        call dispatch_sgr(p, term)

      case (114)  ! 'r' - DECSTBM (set scroll region)
        if (p%param_count == 0) then
          call terminal_set_scroll_region(term, 1, term%rows)
        else if (p%param_count == 1) then
          call terminal_set_scroll_region(term, n, term%rows)
        else
          call terminal_set_scroll_region(term, n, m)
        end if

      case (99)  ! 'c' - DA1 (Primary Device Attributes)
        ! Respond with VT102 identification: ESC [ ? 6 c
        if (p%param_count == 0 .or. (p%param_count == 1 .and. p%params(1) == 0)) then
          call terminal_queue_response(term, char(27) // '[?6c')
        end if

      case (110)  ! 'n' - DSR (Device Status Report)
        if (n == 5) then
          ! Status report: respond "OK" (CSI 0 n)
          call terminal_queue_response(term, char(27) // '[0n')
        else if (n == 6) then
          ! Cursor position report: respond CSI row ; col R
          call terminal_queue_response(term, char(27) // '[' // &
            trim(int_to_str(term%cursor%row)) // ';' // &
            trim(int_to_str(term%cursor%col)) // 'R')
        end if

      case (115)  ! 's' - SCOSC (save cursor position)
        call terminal_save_cursor(term)

      case (117)  ! 'u' - SCORC (restore cursor position)
        call terminal_restore_cursor(term)

      case default
        ! Unknown CSI command - ignore
    end select
  end subroutine dispatch_csi

  ! Erase n characters at cursor (for ECH)
  subroutine terminal_erase_chars(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: col, end_col

    scr => terminal_active_screen(term)
    end_col = min(term%cursor%col + n - 1, term%cols)

    do col = term%cursor%col, end_col
      scr%cells(term%cursor%row, col) = cell_t(32, default_fg, default_bg, 0)
    end do

    call screen_mark_dirty(scr, term%cursor%row)
  end subroutine terminal_erase_chars

  ! Dispatch DEC private mode sequences (CSI ? n h/l)
  subroutine dispatch_dec_private(p, term, cmd, mode)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: cmd, mode
    logical :: set_mode

    ! 'h' = 104 (set mode), 'l' = 108 (reset mode)
    if (cmd == 104) then
      set_mode = .true.
    else if (cmd == 108) then
      set_mode = .false.
    else
      return  ! Not a mode setting command
    end if

    select case (mode)
      case (1)  ! DECCKM - Application cursor keys
        ! We would need to track this mode for cursor key handling
        ! For now, ignore

      case (7)  ! DECAWM - Auto-wrap mode
        term%mode_autowrap = set_mode

      case (12)  ! Cursor blink
        term%cursor%blink = set_mode

      case (25)  ! DECTCEM - Cursor visible
        term%cursor%visible = set_mode

      case (47)  ! Alternate screen buffer (older)
        call terminal_switch_screen(term, set_mode)

      case (1047)  ! Alternate screen buffer
        call terminal_switch_screen(term, set_mode)

      case (1048)  ! Save/restore cursor
        if (set_mode) then
          call terminal_save_cursor(term)
        else
          call terminal_restore_cursor(term)
        end if

      case (1049)  ! Alternate screen + save/restore cursor
        if (set_mode) then
          call terminal_save_cursor(term)
          call terminal_switch_screen(term, .true.)
          call terminal_erase_display(term, 2)
        else
          call terminal_switch_screen(term, .false.)
          call terminal_restore_cursor(term)
        end if

      case (2004)  ! Bracketed paste mode
        ! We would need to track this for paste handling
        ! For now, ignore

      case default
        ! Unknown mode - ignore
    end select
  end subroutine dispatch_dec_private

  ! Dispatch SGR (Select Graphic Rendition)
  subroutine dispatch_sgr(p, term)
    type(parser_t), intent(inout) :: p
    type(terminal_t), intent(inout) :: term
    integer :: i, param
    type(color_t) :: c

    ! Default: SGR 0 (reset)
    if (p%param_count == 0) then
      term%cursor%fg = default_fg
      term%cursor%bg = default_bg
      term%cursor%attrs = 0
      return
    end if

    i = 1
    do while (i <= p%param_count)
      param = p%params(i)

      select case (param)
        case (0)  ! Reset all
          term%cursor%fg = default_fg
          term%cursor%bg = default_bg
          term%cursor%attrs = 0

        case (1)  ! Bold
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_BOLD)

        case (2)  ! Dim/faint (treat as removing bold)
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_BOLD))

        case (3)  ! Italic
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_ITALIC)

        case (4)  ! Underline
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_UNDERLINE)

        case (5, 6)  ! Blink (slow/rapid)
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_BLINK)

        case (7)  ! Inverse
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_INVERSE)

        case (8)  ! Hidden
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_HIDDEN)

        case (9)  ! Strikethrough
          term%cursor%attrs = ior(term%cursor%attrs, ATTR_STRIKETHROUGH)

        case (22)  ! Normal intensity (not bold/dim)
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_BOLD))

        case (23)  ! Not italic
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_ITALIC))

        case (24)  ! Not underlined
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_UNDERLINE))

        case (25)  ! Not blinking
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_BLINK))

        case (27)  ! Not inverse
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_INVERSE))

        case (28)  ! Not hidden
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_HIDDEN))

        case (29)  ! Not strikethrough
          term%cursor%attrs = iand(term%cursor%attrs, not(ATTR_STRIKETHROUGH))

        case (30:37)  ! Standard foreground colors
          term%cursor%fg = color_from_index(param - 30)

        case (38)  ! Extended foreground
          if (i + 1 <= p%param_count) then
            if (p%params(i + 1) == 5 .and. i + 2 <= p%param_count) then
              ! 256-color: 38;5;n
              term%cursor%fg = color_from_index(p%params(i + 2))
              i = i + 2
            else if (p%params(i + 1) == 2 .and. i + 4 <= p%param_count) then
              ! RGB: 38;2;r;g;b
              term%cursor%fg = color_from_rgb(p%params(i + 2), p%params(i + 3), p%params(i + 4))
              i = i + 4
            end if
          end if

        case (39)  ! Default foreground
          term%cursor%fg = default_fg

        case (40:47)  ! Standard background colors
          term%cursor%bg = color_from_index(param - 40)

        case (48)  ! Extended background
          if (i + 1 <= p%param_count) then
            if (p%params(i + 1) == 5 .and. i + 2 <= p%param_count) then
              ! 256-color: 48;5;n
              term%cursor%bg = color_from_index(p%params(i + 2))
              i = i + 2
            else if (p%params(i + 1) == 2 .and. i + 4 <= p%param_count) then
              ! RGB: 48;2;r;g;b
              term%cursor%bg = color_from_rgb(p%params(i + 2), p%params(i + 3), p%params(i + 4))
              i = i + 4
            end if
          end if

        case (49)  ! Default background
          term%cursor%bg = default_bg

        case (90:97)  ! Bright foreground colors
          term%cursor%fg = color_from_index(param - 90 + 8)

        case (100:107)  ! Bright background colors
          term%cursor%bg = color_from_index(param - 100 + 8)

        case default
          ! Unknown SGR parameter - ignore
      end select

      i = i + 1
    end do
  end subroutine dispatch_sgr

  ! Convert integer to string
  function int_to_str(n) result(str)
    integer, intent(in) :: n
    character(len=12) :: str

    write(str, '(I0)') n
  end function int_to_str

end module parser_mod
