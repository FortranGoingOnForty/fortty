module glfw_bindings
  use, intrinsic :: iso_c_binding
  use types
  implicit none
  public

  ! GLFW constants
  integer(c_int), parameter :: GLFW_TRUE = 1
  integer(c_int), parameter :: GLFW_FALSE = 0

  ! Window hints
  integer(c_int), parameter :: GLFW_CONTEXT_VERSION_MAJOR = int(Z'00022002', c_int)
  integer(c_int), parameter :: GLFW_CONTEXT_VERSION_MINOR = int(Z'00022003', c_int)
  integer(c_int), parameter :: GLFW_OPENGL_PROFILE = int(Z'00022008', c_int)
  integer(c_int), parameter :: GLFW_OPENGL_CORE_PROFILE = int(Z'00032001', c_int)
  integer(c_int), parameter :: GLFW_OPENGL_FORWARD_COMPAT = int(Z'00022006', c_int)
  integer(c_int), parameter :: GLFW_TRANSPARENT_FRAMEBUFFER = int(Z'0002000A', c_int)

  ! Key constants
  integer(c_int), parameter :: GLFW_KEY_ESCAPE = 256
  integer(c_int), parameter :: GLFW_KEY_ENTER = 257
  integer(c_int), parameter :: GLFW_KEY_TAB = 258
  integer(c_int), parameter :: GLFW_KEY_BACKSPACE = 259
  integer(c_int), parameter :: GLFW_KEY_INSERT = 260
  integer(c_int), parameter :: GLFW_KEY_DELETE = 261
  integer(c_int), parameter :: GLFW_KEY_RIGHT = 262
  integer(c_int), parameter :: GLFW_KEY_LEFT = 263
  integer(c_int), parameter :: GLFW_KEY_DOWN = 264
  integer(c_int), parameter :: GLFW_KEY_UP = 265
  integer(c_int), parameter :: GLFW_KEY_PAGE_UP = 266
  integer(c_int), parameter :: GLFW_KEY_PAGE_DOWN = 267
  integer(c_int), parameter :: GLFW_KEY_HOME = 268
  integer(c_int), parameter :: GLFW_KEY_END = 269

  ! Function keys
  integer(c_int), parameter :: GLFW_KEY_F1 = 290
  integer(c_int), parameter :: GLFW_KEY_F2 = 291
  integer(c_int), parameter :: GLFW_KEY_F3 = 292
  integer(c_int), parameter :: GLFW_KEY_F4 = 293
  integer(c_int), parameter :: GLFW_KEY_F5 = 294
  integer(c_int), parameter :: GLFW_KEY_F6 = 295
  integer(c_int), parameter :: GLFW_KEY_F7 = 296
  integer(c_int), parameter :: GLFW_KEY_F8 = 297
  integer(c_int), parameter :: GLFW_KEY_F9 = 298
  integer(c_int), parameter :: GLFW_KEY_F10 = 299
  integer(c_int), parameter :: GLFW_KEY_F11 = 300
  integer(c_int), parameter :: GLFW_KEY_F12 = 301

  ! Letter keys (for Ctrl combinations)
  integer(c_int), parameter :: GLFW_KEY_A = 65
  integer(c_int), parameter :: GLFW_KEY_B = 66
  integer(c_int), parameter :: GLFW_KEY_C = 67
  integer(c_int), parameter :: GLFW_KEY_D = 68
  integer(c_int), parameter :: GLFW_KEY_E = 69
  integer(c_int), parameter :: GLFW_KEY_F = 70
  integer(c_int), parameter :: GLFW_KEY_G = 71
  integer(c_int), parameter :: GLFW_KEY_H = 72
  integer(c_int), parameter :: GLFW_KEY_I = 73
  integer(c_int), parameter :: GLFW_KEY_J = 74
  integer(c_int), parameter :: GLFW_KEY_K = 75
  integer(c_int), parameter :: GLFW_KEY_L = 76
  integer(c_int), parameter :: GLFW_KEY_M = 77
  integer(c_int), parameter :: GLFW_KEY_N = 78
  integer(c_int), parameter :: GLFW_KEY_O = 79
  integer(c_int), parameter :: GLFW_KEY_P = 80
  integer(c_int), parameter :: GLFW_KEY_Q = 81
  integer(c_int), parameter :: GLFW_KEY_R = 82
  integer(c_int), parameter :: GLFW_KEY_S = 83
  integer(c_int), parameter :: GLFW_KEY_T = 84
  integer(c_int), parameter :: GLFW_KEY_U = 85
  integer(c_int), parameter :: GLFW_KEY_V = 86
  integer(c_int), parameter :: GLFW_KEY_W = 87
  integer(c_int), parameter :: GLFW_KEY_X = 88
  integer(c_int), parameter :: GLFW_KEY_Y = 89
  integer(c_int), parameter :: GLFW_KEY_Z = 90

  ! Number keys
  integer(c_int), parameter :: GLFW_KEY_0 = 48

  ! Symbol keys (for font size adjustment)
  integer(c_int), parameter :: GLFW_KEY_MINUS = 45       ! '-' key
  integer(c_int), parameter :: GLFW_KEY_EQUAL = 61       ! '=' key

  ! Numpad keys
  integer(c_int), parameter :: GLFW_KEY_KP_SUBTRACT = 333
  integer(c_int), parameter :: GLFW_KEY_KP_ADD = 334

  ! Bracket and backslash keys (for tab/pane keybindings)
  integer(c_int), parameter :: GLFW_KEY_LEFT_BRACKET = 91   ! [
  integer(c_int), parameter :: GLFW_KEY_BACKSLASH = 92      ! \
  integer(c_int), parameter :: GLFW_KEY_RIGHT_BRACKET = 93  ! ]

  ! Number keys 1-9 (for tab switching)
  integer(c_int), parameter :: GLFW_KEY_1 = 49
  integer(c_int), parameter :: GLFW_KEY_2 = 50
  integer(c_int), parameter :: GLFW_KEY_3 = 51
  integer(c_int), parameter :: GLFW_KEY_4 = 52
  integer(c_int), parameter :: GLFW_KEY_5 = 53
  integer(c_int), parameter :: GLFW_KEY_6 = 54
  integer(c_int), parameter :: GLFW_KEY_7 = 55
  integer(c_int), parameter :: GLFW_KEY_8 = 56
  integer(c_int), parameter :: GLFW_KEY_9 = 57

  ! Modifier masks
  integer(c_int), parameter :: GLFW_MOD_SHIFT = 1
  integer(c_int), parameter :: GLFW_MOD_CONTROL = 2
  integer(c_int), parameter :: GLFW_MOD_ALT = 4
  integer(c_int), parameter :: GLFW_MOD_SUPER = 8

  ! Action constants
  integer(c_int), parameter :: GLFW_RELEASE = 0
  integer(c_int), parameter :: GLFW_PRESS = 1
  integer(c_int), parameter :: GLFW_REPEAT = 2

  ! Mouse button constants
  integer(c_int), parameter :: GLFW_MOUSE_BUTTON_LEFT = 0
  integer(c_int), parameter :: GLFW_MOUSE_BUTTON_RIGHT = 1
  integer(c_int), parameter :: GLFW_MOUSE_BUTTON_MIDDLE = 2

  ! Callback type for framebuffer size
  abstract interface
    subroutine glfw_framebuffer_size_callback(window, width, height) bind(C)
      import :: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: width, height
    end subroutine glfw_framebuffer_size_callback

    subroutine glfw_key_callback(window, key, scancode, action, mods) bind(C)
      import :: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: key, scancode, action, mods
    end subroutine glfw_key_callback

    subroutine glfw_error_callback(error_code, description) bind(C)
      import :: c_int, c_ptr
      integer(c_int), value :: error_code
      type(c_ptr), value :: description
    end subroutine glfw_error_callback

    subroutine glfw_char_callback(window, codepoint) bind(C)
      import :: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: codepoint
    end subroutine glfw_char_callback

    subroutine glfw_scroll_callback(window, xoffset, yoffset) bind(C)
      import :: c_ptr, c_double
      type(c_ptr), value :: window
      real(c_double), value :: xoffset, yoffset
    end subroutine glfw_scroll_callback

    subroutine glfw_mouse_button_callback(window, button, action, mods) bind(C)
      import :: c_ptr, c_int
      type(c_ptr), value :: window
      integer(c_int), value :: button, action, mods
    end subroutine glfw_mouse_button_callback

    subroutine glfw_cursor_pos_callback(window, xpos, ypos) bind(C)
      import :: c_ptr, c_double
      type(c_ptr), value :: window
      real(c_double), value :: xpos, ypos
    end subroutine glfw_cursor_pos_callback

    subroutine glfw_window_refresh_callback(window) bind(C)
      import :: c_ptr
      type(c_ptr), value :: window
    end subroutine glfw_window_refresh_callback
  end interface

  interface
    ! int glfwInit(void)
    integer(c_int) function glfwInit() bind(C, name="glfwInit")
      import :: c_int
    end function glfwInit

    ! void glfwTerminate(void)
    subroutine glfwTerminate() bind(C, name="glfwTerminate")
    end subroutine glfwTerminate

    ! void glfwWindowHint(int hint, int value)
    subroutine glfwWindowHint(hint, value) bind(C, name="glfwWindowHint")
      import :: c_int
      integer(c_int), value :: hint, value
    end subroutine glfwWindowHint

    ! GLFWwindow* glfwCreateWindow(int width, int height, const char* title,
    !                              GLFWmonitor* monitor, GLFWwindow* share)
    type(c_ptr) function glfwCreateWindow(width, height, title, monitor, share) &
        bind(C, name="glfwCreateWindow")
      import :: c_int, c_ptr, c_char
      integer(c_int), value :: width, height
      character(kind=c_char), intent(in) :: title(*)
      type(c_ptr), value :: monitor, share
    end function glfwCreateWindow

    ! void glfwDestroyWindow(GLFWwindow* window)
    subroutine glfwDestroyWindow(window) bind(C, name="glfwDestroyWindow")
      import :: c_ptr
      type(c_ptr), value :: window
    end subroutine glfwDestroyWindow

    ! void glfwMakeContextCurrent(GLFWwindow* window)
    subroutine glfwMakeContextCurrent(window) bind(C, name="glfwMakeContextCurrent")
      import :: c_ptr
      type(c_ptr), value :: window
    end subroutine glfwMakeContextCurrent

    ! int glfwWindowShouldClose(GLFWwindow* window)
    integer(c_int) function glfwWindowShouldClose(window) bind(C, name="glfwWindowShouldClose")
      import :: c_int, c_ptr
      type(c_ptr), value :: window
    end function glfwWindowShouldClose

    ! void glfwSetWindowShouldClose(GLFWwindow* window, int value)
    subroutine glfwSetWindowShouldClose(window, value) bind(C, name="glfwSetWindowShouldClose")
      import :: c_int, c_ptr
      type(c_ptr), value :: window
      integer(c_int), value :: value
    end subroutine glfwSetWindowShouldClose

    ! void glfwSetWindowTitle(GLFWwindow* window, const char* title)
    subroutine glfwSetWindowTitle(window, title) bind(C, name="glfwSetWindowTitle")
      import :: c_ptr, c_char
      type(c_ptr), value :: window
      character(kind=c_char), intent(in) :: title(*)
    end subroutine glfwSetWindowTitle

    ! void glfwSwapBuffers(GLFWwindow* window)
    subroutine glfwSwapBuffers(window) bind(C, name="glfwSwapBuffers")
      import :: c_ptr
      type(c_ptr), value :: window
    end subroutine glfwSwapBuffers

    ! void glfwPollEvents(void)
    subroutine glfwPollEvents() bind(C, name="glfwPollEvents")
    end subroutine glfwPollEvents

    ! void glfwWaitEventsTimeout(double timeout)
    ! Waits for events with timeout in seconds - critical for Wayland compositor
    ! responsiveness when window is on inactive workspace
    subroutine glfwWaitEventsTimeout(timeout) bind(C, name="glfwWaitEventsTimeout")
      import :: c_double
      real(c_double), value :: timeout
    end subroutine glfwWaitEventsTimeout

    ! double glfwGetTime(void)
    real(c_double) function glfwGetTime() bind(C, name="glfwGetTime")
      import :: c_double
    end function glfwGetTime

    ! void glfwGetFramebufferSize(GLFWwindow* window, int* width, int* height)
    subroutine glfwGetFramebufferSize(window, width, height) bind(C, name="glfwGetFramebufferSize")
      import :: c_int, c_ptr
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: width, height
    end subroutine glfwGetFramebufferSize

    ! void glfwGetWindowSize(GLFWwindow* window, int* width, int* height)
    subroutine glfwGetWindowSize(window, width, height) bind(C, name="glfwGetWindowSize")
      import :: c_int, c_ptr
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: width, height
    end subroutine glfwGetWindowSize

    ! GLFWframebuffersizefun glfwSetFramebufferSizeCallback(GLFWwindow* window,
    !                                                       GLFWframebuffersizefun callback)
    type(c_funptr) function glfwSetFramebufferSizeCallback(window, callback) &
        bind(C, name="glfwSetFramebufferSizeCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetFramebufferSizeCallback

    ! GLFWkeyfun glfwSetKeyCallback(GLFWwindow* window, GLFWkeyfun callback)
    type(c_funptr) function glfwSetKeyCallback(window, callback) &
        bind(C, name="glfwSetKeyCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetKeyCallback

    ! GLFWcharfun glfwSetCharCallback(GLFWwindow* window, GLFWcharfun callback)
    type(c_funptr) function glfwSetCharCallback(window, callback) &
        bind(C, name="glfwSetCharCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetCharCallback

    ! GLFWscrollfun glfwSetScrollCallback(GLFWwindow* window, GLFWscrollfun callback)
    type(c_funptr) function glfwSetScrollCallback(window, callback) &
        bind(C, name="glfwSetScrollCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetScrollCallback

    ! GLFWerrorfun glfwSetErrorCallback(GLFWerrorfun callback)
    type(c_funptr) function glfwSetErrorCallback(callback) &
        bind(C, name="glfwSetErrorCallback")
      import :: c_funptr
      type(c_funptr), value :: callback
    end function glfwSetErrorCallback

    ! GLFWglproc glfwGetProcAddress(const char* procname)
    type(c_funptr) function glfwGetProcAddress(procname) bind(C, name="glfwGetProcAddress")
      import :: c_funptr, c_char
      character(kind=c_char), intent(in) :: procname(*)
    end function glfwGetProcAddress

    ! GLFWmousebuttonfun glfwSetMouseButtonCallback(GLFWwindow* window, GLFWmousebuttonfun callback)
    type(c_funptr) function glfwSetMouseButtonCallback(window, callback) &
        bind(C, name="glfwSetMouseButtonCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetMouseButtonCallback

    ! GLFWcursorposfun glfwSetCursorPosCallback(GLFWwindow* window, GLFWcursorposfun callback)
    type(c_funptr) function glfwSetCursorPosCallback(window, callback) &
        bind(C, name="glfwSetCursorPosCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetCursorPosCallback

    ! void glfwGetCursorPos(GLFWwindow* window, double* xpos, double* ypos)
    subroutine glfwGetCursorPos(window, xpos, ypos) bind(C, name="glfwGetCursorPos")
      import :: c_ptr, c_double
      type(c_ptr), value :: window
      real(c_double), intent(out) :: xpos, ypos
    end subroutine glfwGetCursorPos

    ! const char* glfwGetClipboardString(GLFWwindow* window)
    type(c_ptr) function glfwGetClipboardString(window) bind(C, name="glfwGetClipboardString")
      import :: c_ptr
      type(c_ptr), value :: window
    end function glfwGetClipboardString

    ! void glfwSetClipboardString(GLFWwindow* window, const char* string)
    subroutine glfwSetClipboardString(window, string) bind(C, name="glfwSetClipboardString")
      import :: c_ptr, c_char
      type(c_ptr), value :: window
      character(kind=c_char), intent(in) :: string(*)
    end subroutine glfwSetClipboardString

    ! GLFWwindowrefreshfun glfwSetWindowRefreshCallback(GLFWwindow* window, GLFWwindowrefreshfun callback)
    type(c_funptr) function glfwSetWindowRefreshCallback(window, callback) &
        bind(C, name="glfwSetWindowRefreshCallback")
      import :: c_ptr, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end function glfwSetWindowRefreshCallback
  end interface

end module glfw_bindings
