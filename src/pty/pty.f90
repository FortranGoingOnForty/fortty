module pty_mod
  use, intrinsic :: iso_c_binding
  use pty_bindings
  implicit none
  private

  public :: pty_t
  public :: pty_open, pty_close, pty_resize
  public :: pty_read, pty_write, pty_is_alive

  type :: pty_t
    integer :: master_fd = -1
    integer :: rows = 24
    integer :: cols = 80
    logical :: active = .false.
  end type pty_t

contains

  ! Open PTY and spawn shell
  function pty_open(shell, rows, cols) result(p)
    character(len=*), intent(in) :: shell
    integer, intent(in) :: rows, cols
    type(pty_t) :: p
    integer(c_int) :: fd

    p%rows = rows
    p%cols = cols

    ! Fork shell with PTY
    fd = c_pty_fork(trim(shell) // c_null_char, int(rows, c_int), int(cols, c_int))

    if (fd < 0) then
      print *, "Error: Failed to create PTY"
      p%active = .false.
      return
    end if

    p%master_fd = fd
    p%active = .true.

  end function pty_open

  ! Close PTY and cleanup
  subroutine pty_close(p)
    type(pty_t), intent(inout) :: p

    if (p%master_fd >= 0) then
      call c_pty_close(int(p%master_fd, c_int))
      p%master_fd = -1
    end if

    p%active = .false.

  end subroutine pty_close

  ! Resize PTY
  subroutine pty_resize(p, rows, cols)
    type(pty_t), intent(inout) :: p
    integer, intent(in) :: rows, cols
    integer(c_int) :: result

    if (.not. p%active) return

    result = c_pty_set_size(int(p%master_fd, c_int), &
                            int(rows, c_int), int(cols, c_int))

    if (result == 0) then
      p%rows = rows
      p%cols = cols
    end if

  end subroutine pty_resize

  ! Read from PTY (non-blocking)
  ! Returns: bytes read (>0), 0 if no data, -1 on error/EOF
  function pty_read(p, buffer, maxlen) result(nbytes)
    type(pty_t), intent(in) :: p
    character(len=*), intent(out) :: buffer
    integer, intent(in) :: maxlen
    integer :: nbytes

    if (.not. p%active) then
      nbytes = -1
      return
    end if

    nbytes = c_pty_read(int(p%master_fd, c_int), buffer, int(maxlen, c_int))

  end function pty_read

  ! Write to PTY
  subroutine pty_write(p, data, length)
    type(pty_t), intent(in) :: p
    character(len=*), intent(in) :: data
    integer, intent(in) :: length
    integer(c_int) :: result

    if (.not. p%active) return

    result = c_pty_write(int(p%master_fd, c_int), data, int(length, c_int))

  end subroutine pty_write

  ! Check if child shell is alive
  function pty_is_alive(p) result(alive)
    type(pty_t), intent(inout) :: p
    logical :: alive
    integer(c_int) :: status

    if (.not. p%active) then
      alive = .false.
      return
    end if

    status = c_pty_child_alive()
    alive = (status /= 0)

    ! Update active flag if child died
    if (.not. alive) then
      p%active = .false.
    end if

  end function pty_is_alive

end module pty_mod
