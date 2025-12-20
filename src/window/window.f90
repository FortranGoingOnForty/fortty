module window_mod
  use, intrinsic :: iso_c_binding
  use types
  use glfw_bindings
  use gl_bindings
  use pty_mod
  use terminal_mod
  implicit none
  private

  public :: window_t
  public :: window_create, window_destroy
  public :: window_should_close, window_swap_buffers, window_poll_events
  public :: window_get_size, window_set_pty, window_set_terminal

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

  ! Interface to C helper for loading OpenGL
  interface
    integer(c_int) function fortty_load_gl() bind(C, name="fortty_load_gl")
      import :: c_int
    end function fortty_load_gl
  end interface

contains

  function window_create(width, height, title) result(win)
    integer, intent(in) :: width, height
    character(len=*), intent(in) :: title
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
    dummy = glfwSetKeyCallback(win%handle, c_funloc(key_callback))
    dummy = glfwSetCharCallback(win%handle, c_funloc(char_callback))
    dummy = glfwSetScrollCallback(win%handle, c_funloc(scroll_callback))

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

  subroutine window_swap_buffers(win)
    type(window_t), intent(in) :: win

    call glfwSwapBuffers(win%handle)
  end subroutine window_swap_buffers

  subroutine window_poll_events()
    call glfwPollEvents()
  end subroutine window_poll_events

  ! Get current framebuffer size
  subroutine window_get_size(win, width, height)
    type(window_t), intent(in) :: win
    integer, intent(out) :: width, height
    integer(c_int) :: w, h

    call glfwGetFramebufferSize(win%handle, w, h)
    width = int(w)
    height = int(h)
  end subroutine window_get_size

  ! Callback: handle window resize
  subroutine framebuffer_size_callback(window, width, height) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: width, height

    call glViewport(0, 0, width, height)
  end subroutine framebuffer_size_callback

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

end module window_mod
