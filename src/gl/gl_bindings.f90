module gl_bindings
  use, intrinsic :: iso_c_binding
  use types
  implicit none
  public

  ! OpenGL constants
  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT = int(Z'00004000', c_int)
  integer(c_int), parameter :: GL_DEPTH_BUFFER_BIT = int(Z'00000100', c_int)

  ! Texture constants
  integer(c_int), parameter :: GL_TEXTURE_2D = int(Z'0DE1', c_int)
  integer(c_int), parameter :: GL_TEXTURE0 = int(Z'84C0', c_int)
  integer(c_int), parameter :: GL_TEXTURE_MIN_FILTER = int(Z'2801', c_int)
  integer(c_int), parameter :: GL_TEXTURE_MAG_FILTER = int(Z'2800', c_int)
  integer(c_int), parameter :: GL_TEXTURE_WRAP_S = int(Z'2802', c_int)
  integer(c_int), parameter :: GL_TEXTURE_WRAP_T = int(Z'2803', c_int)
  integer(c_int), parameter :: GL_LINEAR = int(Z'2601', c_int)
  integer(c_int), parameter :: GL_NEAREST = int(Z'2600', c_int)
  integer(c_int), parameter :: GL_CLAMP_TO_EDGE = int(Z'812F', c_int)
  integer(c_int), parameter :: GL_RED = int(Z'1903', c_int)
  integer(c_int), parameter :: GL_UNSIGNED_BYTE = int(Z'1401', c_int)
  integer(c_int), parameter :: GL_UNPACK_ALIGNMENT = int(Z'0CF5', c_int)

  ! Shader constants
  integer(c_int), parameter :: GL_VERTEX_SHADER = int(Z'8B31', c_int)
  integer(c_int), parameter :: GL_FRAGMENT_SHADER = int(Z'8B30', c_int)

  ! Buffer constants
  integer(c_int), parameter :: GL_ARRAY_BUFFER = int(Z'8892', c_int)
  integer(c_int), parameter :: GL_STATIC_DRAW = int(Z'88E4', c_int)
  integer(c_int), parameter :: GL_DYNAMIC_DRAW = int(Z'88E8', c_int)
  integer(c_int), parameter :: GL_STREAM_DRAW = int(Z'88E0', c_int)

  ! Drawing constants
  integer(c_int), parameter :: GL_TRIANGLES = int(Z'0004', c_int)
  integer(c_int), parameter :: GL_FLOAT = int(Z'1406', c_int)
  integer(c_int), parameter :: GL_FALSE = 0
  integer(c_int), parameter :: GL_TRUE = 1

  ! Blending constants
  integer(c_int), parameter :: GL_BLEND = int(Z'0BE2', c_int)
  integer(c_int), parameter :: GL_SRC_ALPHA = int(Z'0302', c_int)
  integer(c_int), parameter :: GL_ONE_MINUS_SRC_ALPHA = int(Z'0303', c_int)

  ! Scissor test constant
  integer(c_int), parameter :: GL_SCISSOR_TEST = int(Z'0C11', c_int)

  ! Interface to C wrapper functions (in gl_loader.c)
  interface
    ! ============ Basic functions ============
    subroutine glViewport(x, y, width, height) bind(C, name="fortty_glViewport")
      import :: c_int
      integer(c_int), value :: x, y, width, height
    end subroutine glViewport

    subroutine glClear(mask) bind(C, name="fortty_glClear")
      import :: c_int
      integer(c_int), value :: mask
    end subroutine glClear

    subroutine glClearColor(red, green, blue, alpha) bind(C, name="fortty_glClearColor")
      import :: c_float
      real(c_float), value :: red, green, blue, alpha
    end subroutine glClearColor

    ! ============ Texture functions ============
    subroutine glGenTextures(n, textures) bind(C, name="fortty_glGenTextures")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(out) :: textures(*)
    end subroutine glGenTextures

    subroutine glDeleteTextures(n, textures) bind(C, name="fortty_glDeleteTextures")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(in) :: textures(*)
    end subroutine glDeleteTextures

    subroutine glBindTexture(target, texture) bind(C, name="fortty_glBindTexture")
      import :: c_int
      integer(c_int), value :: target, texture
    end subroutine glBindTexture

    subroutine glTexImage2D(target, level, internalformat, width, height, &
                            border, format, textype, data) bind(C, name="fortty_glTexImage2D")
      import :: c_int, c_ptr
      integer(c_int), value :: target, level, internalformat
      integer(c_int), value :: width, height, border
      integer(c_int), value :: format, textype
      type(c_ptr), value :: data
    end subroutine glTexImage2D

    subroutine glTexSubImage2D(target, level, xoffset, yoffset, width, height, &
                               format, textype, data) bind(C, name="fortty_glTexSubImage2D")
      import :: c_int, c_ptr
      integer(c_int), value :: target, level, xoffset, yoffset
      integer(c_int), value :: width, height, format, textype
      type(c_ptr), value :: data
    end subroutine glTexSubImage2D

    subroutine glTexParameteri(target, pname, param) bind(C, name="fortty_glTexParameteri")
      import :: c_int
      integer(c_int), value :: target, pname, param
    end subroutine glTexParameteri

    subroutine glPixelStorei(pname, param) bind(C, name="fortty_glPixelStorei")
      import :: c_int
      integer(c_int), value :: pname, param
    end subroutine glPixelStorei

    subroutine glActiveTexture(texture) bind(C, name="fortty_glActiveTexture")
      import :: c_int
      integer(c_int), value :: texture
    end subroutine glActiveTexture

    ! ============ Shader functions ============
    integer(c_int) function glCreateShader(shadertype) bind(C, name="fortty_glCreateShader")
      import :: c_int
      integer(c_int), value :: shadertype
    end function glCreateShader

    subroutine glDeleteShader(shader) bind(C, name="fortty_glDeleteShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine glDeleteShader

    subroutine glShaderSource(shader, source) bind(C, name="fortty_glShaderSource")
      import :: c_int, c_char
      integer(c_int), value :: shader
      character(kind=c_char), intent(in) :: source(*)
    end subroutine glShaderSource

    subroutine glCompileShader(shader) bind(C, name="fortty_glCompileShader")
      import :: c_int
      integer(c_int), value :: shader
    end subroutine glCompileShader

    integer(c_int) function glGetShaderCompileStatus(shader) &
        bind(C, name="fortty_glGetShaderCompileStatus")
      import :: c_int
      integer(c_int), value :: shader
    end function glGetShaderCompileStatus

    subroutine glGetShaderInfoLog(shader, log, maxlen) bind(C, name="fortty_glGetShaderInfoLog")
      import :: c_int, c_char
      integer(c_int), value :: shader, maxlen
      character(kind=c_char), intent(out) :: log(*)
    end subroutine glGetShaderInfoLog

    integer(c_int) function glCreateProgram() bind(C, name="fortty_glCreateProgram")
      import :: c_int
    end function glCreateProgram

    subroutine glDeleteProgram(program) bind(C, name="fortty_glDeleteProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine glDeleteProgram

    subroutine glAttachShader(program, shader) bind(C, name="fortty_glAttachShader")
      import :: c_int
      integer(c_int), value :: program, shader
    end subroutine glAttachShader

    subroutine glLinkProgram(program) bind(C, name="fortty_glLinkProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine glLinkProgram

    integer(c_int) function glGetProgramLinkStatus(program) &
        bind(C, name="fortty_glGetProgramLinkStatus")
      import :: c_int
      integer(c_int), value :: program
    end function glGetProgramLinkStatus

    subroutine glGetProgramInfoLog(program, log, maxlen) bind(C, name="fortty_glGetProgramInfoLog")
      import :: c_int, c_char
      integer(c_int), value :: program, maxlen
      character(kind=c_char), intent(out) :: log(*)
    end subroutine glGetProgramInfoLog

    subroutine glUseProgram(program) bind(C, name="fortty_glUseProgram")
      import :: c_int
      integer(c_int), value :: program
    end subroutine glUseProgram

    integer(c_int) function glGetUniformLocation(program, name) &
        bind(C, name="fortty_glGetUniformLocation")
      import :: c_int, c_char
      integer(c_int), value :: program
      character(kind=c_char), intent(in) :: name(*)
    end function glGetUniformLocation

    subroutine glUniform1i(location, value) bind(C, name="fortty_glUniform1i")
      import :: c_int
      integer(c_int), value :: location, value
    end subroutine glUniform1i

    subroutine glUniform1f(location, value) bind(C, name="fortty_glUniform1f")
      import :: c_int, c_float
      integer(c_int), value :: location
      real(c_float), value :: value
    end subroutine glUniform1f

    subroutine glUniform3f(location, v0, v1, v2) bind(C, name="fortty_glUniform3f")
      import :: c_int, c_float
      integer(c_int), value :: location
      real(c_float), value :: v0, v1, v2
    end subroutine glUniform3f

    subroutine glUniform4f(location, v0, v1, v2, v3) bind(C, name="fortty_glUniform4f")
      import :: c_int, c_float
      integer(c_int), value :: location
      real(c_float), value :: v0, v1, v2, v3
    end subroutine glUniform4f

    subroutine glUniformMatrix4fv(location, count, transpose, value) &
        bind(C, name="fortty_glUniformMatrix4fv")
      import :: c_int, c_float
      integer(c_int), value :: location, count, transpose
      real(c_float), intent(in) :: value(*)
    end subroutine glUniformMatrix4fv

    ! ============ VAO/VBO functions ============
    subroutine glGenVertexArrays(n, arrays) bind(C, name="fortty_glGenVertexArrays")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(out) :: arrays(*)
    end subroutine glGenVertexArrays

    subroutine glDeleteVertexArrays(n, arrays) bind(C, name="fortty_glDeleteVertexArrays")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(in) :: arrays(*)
    end subroutine glDeleteVertexArrays

    subroutine glBindVertexArray(array) bind(C, name="fortty_glBindVertexArray")
      import :: c_int
      integer(c_int), value :: array
    end subroutine glBindVertexArray

    subroutine glGenBuffers(n, buffers) bind(C, name="fortty_glGenBuffers")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(out) :: buffers(*)
    end subroutine glGenBuffers

    subroutine glDeleteBuffers(n, buffers) bind(C, name="fortty_glDeleteBuffers")
      import :: c_int
      integer(c_int), value :: n
      integer(c_int), intent(in) :: buffers(*)
    end subroutine glDeleteBuffers

    subroutine glBindBuffer(target, buffer) bind(C, name="fortty_glBindBuffer")
      import :: c_int
      integer(c_int), value :: target, buffer
    end subroutine glBindBuffer

    subroutine glBufferData(target, datasize, data, usage) bind(C, name="fortty_glBufferData")
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: datasize
      type(c_ptr), value :: data
      integer(c_int), value :: usage
    end subroutine glBufferData

    subroutine glBufferSubData(target, offset, datasize, data) bind(C, name="fortty_glBufferSubData")
      import :: c_int, c_size_t, c_ptr
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, datasize
      type(c_ptr), value :: data
    end subroutine glBufferSubData

    subroutine glBufferSubData_floats(target, offset, datasize, data) &
        bind(C, name="fortty_glBufferSubData_floats")
      import :: c_int, c_size_t, c_float
      integer(c_int), value :: target
      integer(c_size_t), value :: offset, datasize
      real(c_float), intent(in) :: data(*)
    end subroutine glBufferSubData_floats

    subroutine glVertexAttribPointer(index, vsize, vtype, normalized, stride, offset) &
        bind(C, name="fortty_glVertexAttribPointer")
      import :: c_int, c_size_t
      integer(c_int), value :: index, vsize, vtype, normalized, stride
      integer(c_size_t), value :: offset
    end subroutine glVertexAttribPointer

    subroutine glEnableVertexAttribArray(index) bind(C, name="fortty_glEnableVertexAttribArray")
      import :: c_int
      integer(c_int), value :: index
    end subroutine glEnableVertexAttribArray

    subroutine glDisableVertexAttribArray(index) bind(C, name="fortty_glDisableVertexAttribArray")
      import :: c_int
      integer(c_int), value :: index
    end subroutine glDisableVertexAttribArray

    ! ============ Drawing functions ============
    subroutine glDrawArrays(mode, first, count) bind(C, name="fortty_glDrawArrays")
      import :: c_int
      integer(c_int), value :: mode, first, count
    end subroutine glDrawArrays

    ! ============ State functions ============
    subroutine glEnable(cap) bind(C, name="fortty_glEnable")
      import :: c_int
      integer(c_int), value :: cap
    end subroutine glEnable

    subroutine glDisable(cap) bind(C, name="fortty_glDisable")
      import :: c_int
      integer(c_int), value :: cap
    end subroutine glDisable

    subroutine glBlendFunc(sfactor, dfactor) bind(C, name="fortty_glBlendFunc")
      import :: c_int
      integer(c_int), value :: sfactor, dfactor
    end subroutine glBlendFunc

    subroutine glScissor(x, y, width, height) bind(C, name="fortty_glScissor")
      import :: c_int
      integer(c_int), value :: x, y, width, height
    end subroutine glScissor
  end interface

end module gl_bindings
