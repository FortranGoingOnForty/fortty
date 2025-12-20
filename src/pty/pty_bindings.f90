module pty_bindings
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: c_pty_fork, c_pty_set_size, c_pty_read, c_pty_write
  public :: c_pty_close, c_pty_child_alive, c_pty_get_child_pid

  interface

    ! Fork shell and return master fd (-1 on error)
    integer(c_int) function c_pty_fork(shell, rows, cols) &
        bind(C, name="fortty_pty_fork")
      import :: c_int, c_char
      character(kind=c_char), intent(in) :: shell(*)
      integer(c_int), value :: rows, cols
    end function c_pty_fork

    ! Set PTY window size (0 on success, -1 on error)
    integer(c_int) function c_pty_set_size(master_fd, rows, cols) &
        bind(C, name="fortty_pty_set_size")
      import :: c_int
      integer(c_int), value :: master_fd, rows, cols
    end function c_pty_set_size

    ! Read from PTY (returns bytes read, 0 if would block, -1 on error/EOF)
    integer(c_int) function c_pty_read(fd, buf, count) &
        bind(C, name="fortty_pty_read")
      import :: c_int, c_char
      integer(c_int), value :: fd, count
      character(kind=c_char), intent(out) :: buf(*)
    end function c_pty_read

    ! Write to PTY (returns bytes written, -1 on error)
    integer(c_int) function c_pty_write(fd, buf, count) &
        bind(C, name="fortty_pty_write")
      import :: c_int, c_char
      integer(c_int), value :: fd, count
      character(kind=c_char), intent(in) :: buf(*)
    end function c_pty_write

    ! Close PTY and wait for child
    subroutine c_pty_close(master_fd) bind(C, name="fortty_pty_close")
      import :: c_int
      integer(c_int), value :: master_fd
    end subroutine c_pty_close

    ! Check if child is alive (1 = alive, 0 = dead)
    integer(c_int) function c_pty_child_alive() &
        bind(C, name="fortty_pty_child_alive")
      import :: c_int
    end function c_pty_child_alive

    ! Get child PID
    integer(c_int) function c_pty_get_child_pid() &
        bind(C, name="fortty_pty_get_child_pid")
      import :: c_int
    end function c_pty_get_child_pid

  end interface

end module pty_bindings
