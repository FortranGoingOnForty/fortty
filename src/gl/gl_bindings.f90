module gl_bindings
  use, intrinsic :: iso_c_binding
  use types
  implicit none
  public

  ! OpenGL constants
  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT = int(Z'00004000', c_int)

  ! OpenGL type aliases
  integer, parameter :: GLint = c_int
  integer, parameter :: GLsizei = c_int
  integer, parameter :: GLfloat = c_float
  integer, parameter :: GLbitfield = c_int

  ! Interface to C wrapper functions (in gl_loader.c)
  ! These wrap the GLAD-loaded OpenGL functions
  interface
    ! void fortty_glViewport(int x, int y, int width, int height)
    subroutine glViewport(x, y, width, height) bind(C, name="fortty_glViewport")
      import :: c_int
      integer(c_int), value :: x, y, width, height
    end subroutine glViewport

    ! void fortty_glClear(unsigned int mask)
    subroutine glClear(mask) bind(C, name="fortty_glClear")
      import :: c_int
      integer(c_int), value :: mask
    end subroutine glClear

    ! void fortty_glClearColor(float red, float green, float blue, float alpha)
    subroutine glClearColor(red, green, blue, alpha) bind(C, name="fortty_glClearColor")
      import :: c_float
      real(c_float), value :: red, green, blue, alpha
    end subroutine glClearColor
  end interface

end module gl_bindings
