module window_mod
  use, intrinsic :: iso_c_binding
  use types
  use glfw_bindings
  use gl_bindings
  use pty_mod
  use terminal_mod
  use selection_mod
  implicit none
  private

  public :: window_t, selection_t
  public :: window_create, window_destroy
  public :: window_should_close, window_swap_buffers, window_poll_events
  public :: window_get_size, window_set_pty, window_set_terminal
  public :: window_set_title, window_set_cell_size, window_set_blur
  public :: window_get_selection, window_clipboard_set, window_clipboard_get
  public :: window_get_font_delta, window_clear_font_delta, window_set_font_size
  public :: window_set_render_callback, window_is_resizing
  public :: window_get_tab_action, window_clear_tab_action
  public :: window_get_pane_action, window_clear_pane_action
  public :: window_is_focused

  ! Tab action constants
  integer, parameter, public :: TAB_ACTION_NONE = 0
  integer, parameter, public :: TAB_ACTION_NEW = 1
  integer, parameter, public :: TAB_ACTION_CLOSE = 2
  integer, parameter, public :: TAB_ACTION_NEXT = 3
  integer, parameter, public :: TAB_ACTION_PREV = 4
  ! TAB_ACTION_GOTO_1 through TAB_ACTION_GOTO_9 are 10-18

  ! Pane action constants
  integer, parameter, public :: PANE_ACTION_NONE = 0
  integer, parameter, public :: PANE_ACTION_SPLIT_V = 1
  integer, parameter, public :: PANE_ACTION_SPLIT_H = 2
  integer, parameter, public :: PANE_ACTION_NAV_LEFT = 3
  integer, parameter, public :: PANE_ACTION_NAV_RIGHT = 4
  integer, parameter, public :: PANE_ACTION_NAV_UP = 5
  integer, parameter, public :: PANE_ACTION_NAV_DOWN = 6

  type :: window_t
    type(c_ptr) :: handle = c_null_ptr
    integer :: width = 0
    integer :: height = 0
    integer :: fb_width = 0
    integer :: fb_height = 0
  end type window_t

  ! Module-level window pointer for callbacks
  type(c_ptr), save :: current_window = c_null_ptr

  ! Module-level PTY pointer for keyboard input
  type(pty_t), pointer, save :: active_pty => null()

  ! Module-level terminal pointer for scrollback
  type(terminal_t), pointer, save :: active_term => null()

  ! Selection state for mouse-based text selection
  type(selection_t), save :: active_selection

  ! Cell dimensions for mouse coordinate conversion
  integer, save :: cell_width = 10
  integer, save :: cell_height = 18

  ! Font size adjustment state
  integer, save :: pending_font_delta = 0   ! +2, -2, or -999 for reset
  integer, save :: current_font_size = 16   ! Track current size

  ! Tab action state
  integer, save :: pending_tab_action = 0   ! 0=none, 1=new, 2=close, 3=next, 4=prev, 10-18=goto

  ! Pane action state
  integer, save :: pending_pane_action = 0  ! 0=none, 1=split_v, 2=split_h, 3-6=nav

  ! Live resize rendering support
  logical, save :: is_resizing = .false.
  procedure(render_callback_interface), pointer, save :: render_callback => null()

  ! Abstract interface for render callback
  abstract interface
    subroutine render_callback_interface()
    end subroutine render_callback_interface
  end interface

  ! Interface to C helper for loading OpenGL
  interface
    integer(c_int) function fortty_load_gl() bind(C, name="fortty_load_gl")
      import :: c_int
    end function fortty_load_gl

    ! macOS window blur (no-op on other platforms)
    subroutine fortty_set_window_blur_c(window, enable) bind(C, name="fortty_set_window_blur")
      import :: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: enable
    end subroutine fortty_set_window_blur_c
  end interface

contains

  function window_create(width, height, title, transparent) result(win)
    integer, intent(in) :: width, height
    character(len=*), intent(in) :: title
    logical, intent(in), optional :: transparent
    type(window_t) :: win
    type(c_funptr) :: dummy
    integer(c_int) :: gl_loaded
    character(len=256) :: c_title

    ! Initialize GLFW
    if (glfwInit() == GLFW_FALSE) then
      print *, "Error: Failed to initialize GLFW"
      stop 1
    end if

    ! Set error callback
    dummy = glfwSetErrorCallback(c_funloc(error_callback))

    ! Request OpenGL 3.3 Core Profile
    call glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    call glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
    call glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
    call glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE)

    ! Enable transparent framebuffer if requested (for window opacity)
    if (present(transparent)) then
      if (transparent) then
        call glfwWindowHint(GLFW_TRANSPARENT_FRAMEBUFFER, GLFW_TRUE)
      end if
    end if

    ! Create window
    c_title = trim(title) // c_null_char
    win%handle = glfwCreateWindow(width, height, c_title, c_null_ptr, c_null_ptr)

    if (.not. c_associated(win%handle)) then
      print *, "Error: Failed to create GLFW window"
      call glfwTerminate()
      stop 1
    end if

    win%width = width
    win%height = height
    current_window = win%handle

    ! Make context current
    call glfwMakeContextCurrent(win%handle)

    ! Load OpenGL functions via GLAD
    gl_loaded = fortty_load_gl()
    if (gl_loaded == 0) then
      print *, "Error: Failed to load OpenGL functions"
      call glfwDestroyWindow(win%handle)
      call glfwTerminate()
      stop 1
    end if

    ! Get actual framebuffer size (may differ on HiDPI)
    call glfwGetFramebufferSize(win%handle, win%fb_width, win%fb_height)

    ! Set initial viewport
    call glViewport(0, 0, win%fb_width, win%fb_height)

    ! Register callbacks
    dummy = glfwSetFramebufferSizeCallback(win%handle, c_funloc(framebuffer_size_callback))
    dummy = glfwSetWindowRefreshCallback(win%handle, c_funloc(window_refresh_callback))
    dummy = glfwSetKeyCallback(win%handle, c_funloc(key_callback))
    dummy = glfwSetCharCallback(win%handle, c_funloc(char_callback))
    dummy = glfwSetScrollCallback(win%handle, c_funloc(scroll_callback))
    dummy = glfwSetMouseButtonCallback(win%handle, c_funloc(mouse_button_callback))
    dummy = glfwSetCursorPosCallback(win%handle, c_funloc(cursor_pos_callback))

  end function window_create

  subroutine window_destroy(win)
    type(window_t), intent(inout) :: win

    if (c_associated(win%handle)) then
      call glfwDestroyWindow(win%handle)
      win%handle = c_null_ptr
    end if

    call glfwTerminate()
  end subroutine window_destroy

  function window_should_close(win) result(should)
    type(window_t), intent(in) :: win
    logical :: should

    should = glfwWindowShouldClose(win%handle) /= GLFW_FALSE
  end function window_should_close

  ! Check if window has focus - used to skip rendering on Wayland when
  ! window is on inactive workspace (prevents compositor timeout)
  function window_is_focused(win) result(focused)
    type(window_t), intent(in) :: win
    logical :: focused

    focused = glfwGetWindowAttrib(win%handle, GLFW_FOCUSED) /= GLFW_FALSE
  end function window_is_focused

  subroutine window_swap_buffers(win)
    type(window_t), intent(in) :: win

    call glfwSwapBuffers(win%handle)
  end subroutine window_swap_buffers

  subroutine window_poll_events()
    ! Use WaitEventsTimeout instead of PollEvents for Wayland compatibility
    ! This allows the compositor to send ping-pong messages to verify the app
    ! is responsive, even when on an inactive workspace. 10ms timeout provides
    ! ~100Hz max update rate while allowing proper event processing.
    call glfwWaitEventsTimeout(0.01d0)
  end subroutine window_poll_events

  ! Get current framebuffer size
  subroutine window_get_size(win, width, height)
    type(window_t), intent(in) :: win
    integer, intent(out) :: width, height
    integer(c_int) :: w, h

    ! Use window size (points) not framebuffer size (pixels)
    ! This ensures projection and cell math work correctly on HiDPI displays
    call glfwGetWindowSize(win%handle, w, h)
    width = int(w)
    height = int(h)
  end subroutine window_get_size

  ! Callback: handle window resize
  subroutine framebuffer_size_callback(window, width, height) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: width, height

    call glViewport(0, 0, width, height)

    ! Mark that we're in a resize operation and trigger a redraw
    ! This ensures smooth rendering during live resize on macOS
    is_resizing = .true.
    if (associated(render_callback)) then
      call render_callback()
    end if
  end subroutine framebuffer_size_callback

  ! Callback: handle window refresh (called when window needs redrawing)
  subroutine window_refresh_callback(window) bind(C)
    type(c_ptr), value :: window

    ! Trigger a redraw via the render callback
    if (associated(render_callback)) then
      call render_callback()
    end if
  end subroutine window_refresh_callback

  ! Set PTY for keyboard input
  subroutine window_set_pty(p)
    type(pty_t), target, intent(in) :: p

    active_pty => p
  end subroutine window_set_pty

  ! Set terminal for scrollback control
  subroutine window_set_terminal(t)
    type(terminal_t), target, intent(in) :: t

    active_term => t
  end subroutine window_set_terminal

  ! Callback: handle key presses (special keys and Ctrl combinations)
  subroutine key_callback(window, key, scancode, action, mods) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: key, scancode, action, mods
    character(len=16) :: seq
    integer :: seq_len

    ! Only process press and repeat events
    if (action == GLFW_RELEASE) return

    ! Check if PTY is active
    if (.not. associated(active_pty)) return

    ! Close window on Escape (keep this for development)
    if (key == GLFW_KEY_ESCAPE .and. action == GLFW_PRESS) then
      call glfwSetWindowShouldClose(window, GLFW_TRUE)
      return
    end if

    ! Handle Shift+PageUp/Down for scrollback (before resetting scroll)
    if (associated(active_term) .and. iand(mods, GLFW_MOD_SHIFT) /= 0) then
      if (key == GLFW_KEY_PAGE_UP) then
        call terminal_scroll_view(active_term, active_term%rows)
        return
      else if (key == GLFW_KEY_PAGE_DOWN) then
        call terminal_scroll_view(active_term, -active_term%rows)
        return
      end if
    end if

    ! Reset scroll view on any other key input (return to live view)
    if (associated(active_term)) then
      call terminal_reset_scroll_view(active_term)
    end if

    ! Handle Ctrl+Shift+V for paste
    if (iand(mods, GLFW_MOD_CONTROL) /= 0 .and. iand(mods, GLFW_MOD_SHIFT) /= 0) then
      if (key == GLFW_KEY_V) then
        call handle_paste(window)
        return
      end if
    end if

    ! Handle font size adjustment: Ctrl/Cmd + Plus/Minus/0
    if (iand(mods, GLFW_MOD_CONTROL) /= 0 .or. iand(mods, GLFW_MOD_SUPER) /= 0) then
      if (key == GLFW_KEY_EQUAL .or. key == GLFW_KEY_KP_ADD) then
        ! Ctrl/Cmd + = or numpad + (increase font size)
        pending_font_delta = 2
        return
      else if (key == GLFW_KEY_MINUS .or. key == GLFW_KEY_KP_SUBTRACT) then
        ! Ctrl/Cmd + - (decrease font size)
        pending_font_delta = -2
        return
      else if (key == GLFW_KEY_0) then
        ! Ctrl/Cmd + 0 (reset font size)
        pending_font_delta = -999
        return
      end if
    end if

    ! Handle tab management: Ctrl/Cmd + T/W/[/]/1-9
    if (iand(mods, GLFW_MOD_CONTROL) /= 0 .or. iand(mods, GLFW_MOD_SUPER) /= 0) then
      select case (key)
        case (GLFW_KEY_T)
          ! New tab
          pending_tab_action = TAB_ACTION_NEW
          return
        case (GLFW_KEY_W)
          ! Close tab
          pending_tab_action = TAB_ACTION_CLOSE
          return
        case (GLFW_KEY_RIGHT_BRACKET)
          ! Next tab (Cmd+])
          pending_tab_action = TAB_ACTION_NEXT
          return
        case (GLFW_KEY_LEFT_BRACKET)
          ! Previous tab (Cmd+[)
          pending_tab_action = TAB_ACTION_PREV
          return
        case (GLFW_KEY_1)
          pending_tab_action = 10
          return
        case (GLFW_KEY_2)
          pending_tab_action = 11
          return
        case (GLFW_KEY_3)
          pending_tab_action = 12
          return
        case (GLFW_KEY_4)
          pending_tab_action = 13
          return
        case (GLFW_KEY_5)
          pending_tab_action = 14
          return
        case (GLFW_KEY_6)
          pending_tab_action = 15
          return
        case (GLFW_KEY_7)
          pending_tab_action = 16
          return
        case (GLFW_KEY_8)
          pending_tab_action = 17
          return
        case (GLFW_KEY_9)
          pending_tab_action = 18
          return

        ! Pane splitting: Cmd/Ctrl + \ (vertical), Cmd/Ctrl + Shift + \ (horizontal)
        case (GLFW_KEY_BACKSLASH)
          if (iand(mods, GLFW_MOD_SHIFT) /= 0) then
            pending_pane_action = PANE_ACTION_SPLIT_H
          else
            pending_pane_action = PANE_ACTION_SPLIT_V
          end if
          return

        ! Pane navigation with arrow keys: Cmd/Ctrl + Arrow
        case (GLFW_KEY_LEFT)
          pending_pane_action = PANE_ACTION_NAV_LEFT
          return
        case (GLFW_KEY_RIGHT)
          pending_pane_action = PANE_ACTION_NAV_RIGHT
          return
        case (GLFW_KEY_UP)
          pending_pane_action = PANE_ACTION_NAV_UP
          return
        case (GLFW_KEY_DOWN)
          pending_pane_action = PANE_ACTION_NAV_DOWN
          return

      end select
    end if

    ! Pane navigation with vim keys: Super/Cmd + hjkl only (not Ctrl)
    ! This allows Ctrl+L (clear), Ctrl+H, etc. to pass through to the shell
    if (iand(mods, GLFW_MOD_SUPER) /= 0 .and. iand(mods, GLFW_MOD_CONTROL) == 0) then
      select case (key)
        case (GLFW_KEY_H)
          pending_pane_action = PANE_ACTION_NAV_LEFT
          return
        case (GLFW_KEY_L)
          pending_pane_action = PANE_ACTION_NAV_RIGHT
          return
        case (GLFW_KEY_K)
          pending_pane_action = PANE_ACTION_NAV_UP
          return
        case (GLFW_KEY_J)
          pending_pane_action = PANE_ACTION_NAV_DOWN
          return
      end select
    end if

    ! Handle Ctrl combinations (these don't trigger char_callback)
    if (iand(mods, GLFW_MOD_CONTROL) /= 0) then
      if (key >= GLFW_KEY_A .and. key <= GLFW_KEY_Z) then
        ! Ctrl+A through Ctrl+Z = 0x01-0x1A
        seq(1:1) = char(key - GLFW_KEY_A + 1)
        call pty_write(active_pty, seq, 1)
        return
      end if
    end if

    ! Handle special keys
    seq_len = 0
    select case (key)
      case (GLFW_KEY_ENTER)
        seq(1:1) = char(13)  ! CR
        seq_len = 1

      case (GLFW_KEY_TAB)
        if (iand(mods, GLFW_MOD_SHIFT) /= 0) then
          ! Shift+Tab = CSI Z
          seq = char(27) // '[Z'
          seq_len = 3
        else
          seq(1:1) = char(9)  ! Tab
          seq_len = 1
        end if

      case (GLFW_KEY_BACKSPACE)
        seq(1:1) = char(127)  ! DEL (most terminals expect this)
        seq_len = 1

      case (GLFW_KEY_UP)
        call build_arrow_seq(seq, seq_len, 'A', mods)

      case (GLFW_KEY_DOWN)
        call build_arrow_seq(seq, seq_len, 'B', mods)

      case (GLFW_KEY_RIGHT)
        call build_arrow_seq(seq, seq_len, 'C', mods)

      case (GLFW_KEY_LEFT)
        call build_arrow_seq(seq, seq_len, 'D', mods)

      case (GLFW_KEY_HOME)
        call build_tilde_seq(seq, seq_len, 1, mods)

      case (GLFW_KEY_END)
        call build_tilde_seq(seq, seq_len, 4, mods)

      case (GLFW_KEY_INSERT)
        call build_tilde_seq(seq, seq_len, 2, mods)

      case (GLFW_KEY_DELETE)
        call build_tilde_seq(seq, seq_len, 3, mods)

      case (GLFW_KEY_PAGE_UP)
        call build_tilde_seq(seq, seq_len, 5, mods)

      case (GLFW_KEY_PAGE_DOWN)
        call build_tilde_seq(seq, seq_len, 6, mods)

      case (GLFW_KEY_F1)
        call build_fkey_seq(seq, seq_len, 11, mods)
      case (GLFW_KEY_F2)
        call build_fkey_seq(seq, seq_len, 12, mods)
      case (GLFW_KEY_F3)
        call build_fkey_seq(seq, seq_len, 13, mods)
      case (GLFW_KEY_F4)
        call build_fkey_seq(seq, seq_len, 14, mods)
      case (GLFW_KEY_F5)
        call build_fkey_seq(seq, seq_len, 15, mods)
      case (GLFW_KEY_F6)
        call build_fkey_seq(seq, seq_len, 17, mods)
      case (GLFW_KEY_F7)
        call build_fkey_seq(seq, seq_len, 18, mods)
      case (GLFW_KEY_F8)
        call build_fkey_seq(seq, seq_len, 19, mods)
      case (GLFW_KEY_F9)
        call build_fkey_seq(seq, seq_len, 20, mods)
      case (GLFW_KEY_F10)
        call build_fkey_seq(seq, seq_len, 21, mods)
      case (GLFW_KEY_F11)
        call build_fkey_seq(seq, seq_len, 23, mods)
      case (GLFW_KEY_F12)
        call build_fkey_seq(seq, seq_len, 24, mods)
    end select

    if (seq_len > 0) then
      call pty_write(active_pty, seq, seq_len)
    end if
  end subroutine key_callback

  ! Callback: handle character input (regular text)
  subroutine char_callback(window, codepoint) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: codepoint
    character(len=4) :: utf8
    integer :: utf8_len

    if (.not. associated(active_pty)) return

    ! Convert Unicode codepoint to UTF-8
    call codepoint_to_utf8(codepoint, utf8, utf8_len)

    if (utf8_len > 0) then
      call pty_write(active_pty, utf8, utf8_len)
    end if
  end subroutine char_callback

  ! Build arrow key escape sequence
  subroutine build_arrow_seq(seq, seq_len, letter, mods)
    character(len=*), intent(out) :: seq
    integer, intent(out) :: seq_len
    character(len=1), intent(in) :: letter
    integer(c_int), intent(in) :: mods
    integer :: mod_num

    mod_num = get_modifier_num(mods)

    if (mod_num == 0) then
      ! ESC [ <letter>
      seq = char(27) // '[' // letter
      seq_len = 3
    else
      ! ESC [ 1 ; <mod> <letter>
      seq = char(27) // '[1;' // char(mod_num + ichar('0')) // letter
      seq_len = 6
    end if
  end subroutine build_arrow_seq

  ! Build tilde-terminated escape sequence (Home, End, Insert, Delete, PgUp, PgDn)
  subroutine build_tilde_seq(seq, seq_len, code, mods)
    character(len=*), intent(out) :: seq
    integer, intent(out) :: seq_len
    integer, intent(in) :: code
    integer(c_int), intent(in) :: mods
    integer :: mod_num
    character(len=2) :: code_str

    mod_num = get_modifier_num(mods)
    write(code_str, '(I1)') code

    if (mod_num == 0) then
      ! ESC [ <code> ~
      seq = char(27) // '[' // trim(code_str) // '~'
      seq_len = 4
    else
      ! ESC [ <code> ; <mod> ~
      seq = char(27) // '[' // trim(code_str) // ';' // char(mod_num + ichar('0')) // '~'
      seq_len = 6
    end if
  end subroutine build_tilde_seq

  ! Build function key escape sequence
  subroutine build_fkey_seq(seq, seq_len, code, mods)
    character(len=*), intent(out) :: seq
    integer, intent(out) :: seq_len
    integer, intent(in) :: code
    integer(c_int), intent(in) :: mods
    integer :: mod_num
    character(len=2) :: code_str

    mod_num = get_modifier_num(mods)
    write(code_str, '(I2)') code

    if (mod_num == 0) then
      ! ESC [ <code> ~
      seq = char(27) // '[' // trim(code_str) // '~'
      seq_len = 5
    else
      ! ESC [ <code> ; <mod> ~
      seq = char(27) // '[' // trim(code_str) // ';' // char(mod_num + ichar('0')) // '~'
      seq_len = 7
    end if
  end subroutine build_fkey_seq

  ! Convert GLFW modifier bits to xterm modifier number
  function get_modifier_num(mods) result(mod_num)
    integer(c_int), intent(in) :: mods
    integer :: mod_num

    mod_num = 0
    if (iand(mods, GLFW_MOD_SHIFT) /= 0) mod_num = mod_num + 1
    if (iand(mods, GLFW_MOD_ALT) /= 0) mod_num = mod_num + 2
    if (iand(mods, GLFW_MOD_CONTROL) /= 0) mod_num = mod_num + 4

    ! xterm encoding: modifier = 1 + shift + alt*2 + ctrl*4
    if (mod_num > 0) mod_num = mod_num + 1
  end function get_modifier_num

  ! Convert Unicode codepoint to UTF-8 bytes
  subroutine codepoint_to_utf8(cp, utf8, length)
    integer(c_int), intent(in) :: cp
    character(len=*), intent(out) :: utf8
    integer, intent(out) :: length

    if (cp < 128) then
      ! ASCII
      utf8(1:1) = char(cp)
      length = 1
    else if (cp < 2048) then
      ! 2-byte UTF-8
      utf8(1:1) = char(ior(192, ishft(cp, -6)))
      utf8(2:2) = char(ior(128, iand(cp, 63)))
      length = 2
    else if (cp < 65536) then
      ! 3-byte UTF-8
      utf8(1:1) = char(ior(224, ishft(cp, -12)))
      utf8(2:2) = char(ior(128, iand(ishft(cp, -6), 63)))
      utf8(3:3) = char(ior(128, iand(cp, 63)))
      length = 3
    else if (cp < 1114112) then
      ! 4-byte UTF-8
      utf8(1:1) = char(ior(240, ishft(cp, -18)))
      utf8(2:2) = char(ior(128, iand(ishft(cp, -12), 63)))
      utf8(3:3) = char(ior(128, iand(ishft(cp, -6), 63)))
      utf8(4:4) = char(ior(128, iand(cp, 63)))
      length = 4
    else
      ! Invalid codepoint
      length = 0
    end if
  end subroutine codepoint_to_utf8

  ! Callback: handle GLFW errors
  subroutine error_callback(error_code, description) bind(C)
    integer(c_int), value :: error_code
    type(c_ptr), value :: description

    print *, "GLFW Error ", error_code
  end subroutine error_callback

  ! Callback: handle mouse scroll wheel
  subroutine scroll_callback(window, xoffset, yoffset) bind(C)
    type(c_ptr), value :: window
    real(c_double), value :: xoffset, yoffset
    integer :: scroll_lines

    if (.not. associated(active_term)) return

    ! Convert scroll amount to lines (typically 3 lines per notch)
    scroll_lines = nint(yoffset * 3.0d0)

    ! Positive yoffset = scroll up (back in history)
    call terminal_scroll_view(active_term, scroll_lines)
  end subroutine scroll_callback

  ! Set window title
  subroutine window_set_title(win, title)
    type(window_t), intent(in) :: win
    character(len=*), intent(in) :: title
    character(len=257) :: c_title

    if (.not. c_associated(win%handle)) return

    c_title = trim(title) // c_null_char
    call glfwSetWindowTitle(win%handle, c_title)
  end subroutine window_set_title

  ! Enable background blur (macOS only, no-op on other platforms)
  subroutine window_set_blur(win, enable)
    type(window_t), intent(in) :: win
    logical, intent(in) :: enable

    if (.not. c_associated(win%handle)) return

    if (enable) then
      call fortty_set_window_blur_c(win%handle, 1_c_int)
    else
      call fortty_set_window_blur_c(win%handle, 0_c_int)
    end if
  end subroutine window_set_blur

  ! Set cell dimensions for mouse coordinate conversion
  subroutine window_set_cell_size(w, h)
    integer, intent(in) :: w, h

    cell_width = w
    cell_height = h
  end subroutine window_set_cell_size

  ! Get selection state (for rendering)
  function window_get_selection() result(sel)
    type(selection_t) :: sel

    sel = active_selection
  end function window_get_selection

  ! Set clipboard content
  subroutine window_clipboard_set(win, text)
    type(window_t), intent(in) :: win
    character(len=*), intent(in) :: text
    character(len=4097) :: c_text

    if (.not. c_associated(win%handle)) return
    if (len_trim(text) == 0) return

    c_text = trim(text) // c_null_char
    call glfwSetClipboardString(win%handle, c_text)
  end subroutine window_clipboard_set

  ! Get clipboard content
  function window_clipboard_get(win) result(text)
    type(window_t), intent(in) :: win
    character(len=4096) :: text
    type(c_ptr) :: clip_ptr
    character(len=1), pointer :: chars(:)
    integer :: i, length

    text = ''
    if (.not. c_associated(win%handle)) return

    clip_ptr = glfwGetClipboardString(win%handle)
    if (.not. c_associated(clip_ptr)) return

    ! Convert C string to Fortran string
    call c_f_pointer(clip_ptr, chars, [4096])
    length = 0
    do i = 1, 4096
      if (chars(i) == c_null_char) exit
      length = i
    end do

    do i = 1, min(length, 4096)
      text(i:i) = chars(i)
    end do
  end function window_clipboard_get

  ! Callback: handle mouse button events
  subroutine mouse_button_callback(window, button, action, mods) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: button, action, mods
    real(c_double) :: xpos, ypos
    integer :: col, row

    ! Only handle left mouse button for selection
    if (button /= GLFW_MOUSE_BUTTON_LEFT) return

    call glfwGetCursorPos(window, xpos, ypos)

    ! Convert pixel position to terminal cell coordinates (1-based)
    col = int(xpos / cell_width) + 1
    row = int(ypos / cell_height) + 1

    ! Clamp to valid range
    if (associated(active_term)) then
      if (col < 1) col = 1
      if (col > active_term%cols) col = active_term%cols
      if (row < 1) row = 1
      if (row > active_term%rows) row = active_term%rows
    end if

    if (action == GLFW_PRESS) then
      call selection_start(active_selection, row, col)
    else if (action == GLFW_RELEASE) then
      call selection_end(active_selection)
      ! Copy selection to clipboard if active
      if (selection_is_active(active_selection)) then
        call copy_selection_to_clipboard(window)
      end if
    end if
  end subroutine mouse_button_callback

  ! Callback: handle cursor position changes
  subroutine cursor_pos_callback(window, xpos, ypos) bind(C)
    type(c_ptr), value :: window
    real(c_double), value :: xpos, ypos
    integer :: col, row

    ! Only update if actively selecting
    if (.not. active_selection%selecting) return

    ! Convert pixel position to terminal cell coordinates (1-based)
    col = int(xpos / cell_width) + 1
    row = int(ypos / cell_height) + 1

    ! Clamp to valid range
    if (associated(active_term)) then
      if (col < 1) col = 1
      if (col > active_term%cols) col = active_term%cols
      if (row < 1) row = 1
      if (row > active_term%rows) row = active_term%rows
    end if

    call selection_update(active_selection, row, col)
  end subroutine cursor_pos_callback

  ! Copy selection to clipboard
  subroutine copy_selection_to_clipboard(window)
    use screen_mod
    use cell_mod
    type(c_ptr), intent(in) :: window
    type(screen_t), pointer :: scr
    type(cell_t) :: cell
    character(len=4096) :: text
    character(len=4) :: utf8
    integer :: r1, c1, r2, c2, row, col, pos, utf8_len

    if (.not. associated(active_term)) return
    if (.not. selection_is_active(active_selection)) return

    scr => terminal_active_screen(active_term)
    call selection_get_bounds(active_selection, r1, c1, r2, c2)

    text = ''
    pos = 1

    do row = r1, r2
      do col = 1, scr%cols
        ! Check if this cell is in selection
        if (.not. selection_contains(active_selection, row, col)) cycle

        cell = screen_get_cell(scr, row, col)

        ! Convert codepoint to UTF-8
        if (cell%codepoint >= 32 .and. cell%codepoint < 1114112) then
          call codepoint_to_utf8(cell%codepoint, utf8, utf8_len)
          if (pos + utf8_len - 1 <= 4096) then
            text(pos:pos+utf8_len-1) = utf8(1:utf8_len)
            pos = pos + utf8_len
          end if
        end if
      end do

      ! Add newline between rows (except last row)
      if (row < r2 .and. pos < 4096) then
        text(pos:pos) = char(10)
        pos = pos + 1
      end if
    end do

    ! Set clipboard
    if (pos > 1) then
      call glfwSetClipboardString(window, trim(text(1:pos-1)) // c_null_char)
    end if
  end subroutine copy_selection_to_clipboard

  ! Handle paste from clipboard
  subroutine handle_paste(window)
    type(c_ptr), intent(in) :: window
    type(c_ptr) :: clip_ptr
    character(len=1), pointer :: chars(:)
    integer :: i, length

    if (.not. associated(active_pty)) return

    clip_ptr = glfwGetClipboardString(window)
    if (.not. c_associated(clip_ptr)) return

    ! Find length of C string
    call c_f_pointer(clip_ptr, chars, [4096])
    length = 0
    do i = 1, 4096
      if (chars(i) == c_null_char) exit
      length = i
    end do

    ! Write to PTY
    if (length > 0) then
      block
        character(len=4096) :: paste_text
        integer :: j
        paste_text = ''
        do j = 1, length
          paste_text(j:j) = chars(j)
        end do
        call pty_write(active_pty, paste_text, length)
      end block
    end if
  end subroutine handle_paste

  ! Get pending font size delta (returns 0 if none)
  function window_get_font_delta() result(delta)
    integer :: delta
    delta = pending_font_delta
  end function window_get_font_delta

  ! Clear pending font size delta
  subroutine window_clear_font_delta()
    pending_font_delta = 0
  end subroutine window_clear_font_delta

  ! Set current font size (for tracking)
  subroutine window_set_font_size(size)
    integer, intent(in) :: size
    current_font_size = size
  end subroutine window_set_font_size

  ! Set the render callback for live resize support
  subroutine window_set_render_callback(callback)
    procedure(render_callback_interface) :: callback

    render_callback => callback
  end subroutine window_set_render_callback

  ! Check if window is currently resizing (for main loop optimization)
  function window_is_resizing() result(resizing)
    logical :: resizing
    resizing = is_resizing
    is_resizing = .false.  ! Clear the flag after reading
  end function window_is_resizing

  ! Get pending tab action (returns 0 if none)
  function window_get_tab_action() result(action)
    integer :: action
    action = pending_tab_action
  end function window_get_tab_action

  ! Clear pending tab action
  subroutine window_clear_tab_action()
    pending_tab_action = TAB_ACTION_NONE
  end subroutine window_clear_tab_action

  ! Get pending pane action (returns 0 if none)
  function window_get_pane_action() result(action)
    integer :: action
    action = pending_pane_action
  end function window_get_pane_action

  ! Clear pending pane action
  subroutine window_clear_pane_action()
    pending_pane_action = PANE_ACTION_NONE
  end subroutine window_clear_pane_action

end module window_mod
