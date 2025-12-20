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

  ! Key constants
  integer(c_int), parameter :: GLFW_KEY_ESCAPE = 256

  ! Action constants
  integer(c_int), parameter :: GLFW_RELEASE = 0
  integer(c_int), parameter :: GLFW_PRESS = 1
  integer(c_int), parameter :: GLFW_REPEAT = 2

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

    ! void glfwSwapBuffers(GLFWwindow* window)
    subroutine glfwSwapBuffers(window) bind(C, name="glfwSwapBuffers")
      import :: c_ptr
      type(c_ptr), value :: window
    end subroutine glfwSwapBuffers

    ! void glfwPollEvents(void)
    subroutine glfwPollEvents() bind(C, name="glfwPollEvents")
    end subroutine glfwPollEvents

    ! void glfwGetFramebufferSize(GLFWwindow* window, int* width, int* height)
    subroutine glfwGetFramebufferSize(window, width, height) bind(C, name="glfwGetFramebufferSize")
      import :: c_int, c_ptr
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: width, height
    end subroutine glfwGetFramebufferSize

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
  end interface

end module glfw_bindings
