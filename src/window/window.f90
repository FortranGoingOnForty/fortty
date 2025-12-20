module window_mod
  use, intrinsic :: iso_c_binding
  use types
  use glfw_bindings
  use gl_bindings
  implicit none
  private

  public :: window_t
  public :: window_create, window_destroy
  public :: window_should_close, window_swap_buffers, window_poll_events
  public :: window_get_size

  type :: window_t
    type(c_ptr) :: handle = c_null_ptr
    integer :: width = 0
    integer :: height = 0
    integer :: fb_width = 0
    integer :: fb_height = 0
  end type window_t

  ! Module-level window pointer for callbacks
  ! (GLFW callbacks don't have user data for framebuffer callback)
  type(c_ptr), save :: current_window = c_null_ptr

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

  ! Callback: handle key presses
  subroutine key_callback(window, key, scancode, action, mods) bind(C)
    type(c_ptr), value :: window
    integer(c_int), value :: key, scancode, action, mods

    ! Close window on Escape
    if (key == GLFW_KEY_ESCAPE .and. action == GLFW_PRESS) then
      call glfwSetWindowShouldClose(window, GLFW_TRUE)
    end if
  end subroutine key_callback

  ! Callback: handle GLFW errors
  subroutine error_callback(error_code, description) bind(C)
    integer(c_int), value :: error_code
    type(c_ptr), value :: description

    print *, "GLFW Error ", error_code
  end subroutine error_callback

end module window_mod
