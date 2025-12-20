module scrollback_mod
  use cell_mod
  implicit none
  private

  public :: scrollback_t
  public :: scrollback_init, scrollback_destroy
  public :: scrollback_push_line, scrollback_get_line
  public :: scrollback_clear, scrollback_count

  integer, parameter :: DEFAULT_CAPACITY = 10000

  type :: scrollback_t
    type(cell_t), allocatable :: lines(:,:)  ! (line_index, column)
    integer :: capacity = 0                   ! Maximum lines to store
    integer :: cols = 0                       ! Columns per line
    integer :: count = 0                      ! Current number of lines stored
    integer :: head = 0                       ! Next write position (0-based circular)
    logical :: initialized = .false.
  end type scrollback_t

contains

  ! Initialize scrollback buffer
  subroutine scrollback_init(sb, cols, capacity)
    type(scrollback_t), intent(inout) :: sb
    integer, intent(in) :: cols
    integer, intent(in), optional :: capacity

    if (present(capacity)) then
      sb%capacity = capacity
    else
      sb%capacity = DEFAULT_CAPACITY
    end if

    sb%cols = cols
    sb%count = 0
    sb%head = 0

    allocate(sb%lines(sb%capacity, cols))

    ! Initialize all cells to spaces
    sb%lines%codepoint = 32
    sb%lines%fg = default_fg
    sb%lines%bg = default_bg
    sb%lines%attrs = 0

    sb%initialized = .true.
  end subroutine scrollback_init

  ! Destroy scrollback buffer
  subroutine scrollback_destroy(sb)
    type(scrollback_t), intent(inout) :: sb

    if (allocated(sb%lines)) deallocate(sb%lines)
    sb%capacity = 0
    sb%cols = 0
    sb%count = 0
    sb%head = 0
    sb%initialized = .false.
  end subroutine scrollback_destroy

  ! Push a line into scrollback (circular buffer)
  subroutine scrollback_push_line(sb, line, cols)
    type(scrollback_t), intent(inout) :: sb
    type(cell_t), intent(in) :: line(:)
    integer, intent(in) :: cols
    integer :: col, copy_cols

    if (.not. sb%initialized) return
    if (sb%capacity <= 0) return

    ! Calculate how many columns to copy
    copy_cols = min(cols, sb%cols, size(line))

    ! Write to current head position (1-based index)
    do col = 1, copy_cols
      sb%lines(sb%head + 1, col) = line(col)
    end do

    ! Clear remaining columns if line is shorter
    do col = copy_cols + 1, sb%cols
      sb%lines(sb%head + 1, col) = cell_t(32, default_fg, default_bg, 0)
    end do

    ! Advance head (circular)
    sb%head = mod(sb%head + 1, sb%capacity)

    ! Update count (max out at capacity)
    if (sb%count < sb%capacity) then
      sb%count = sb%count + 1
    end if
  end subroutine scrollback_push_line

  ! Get a line from scrollback
  ! offset: 0 = most recent line, 1 = second most recent, etc.
  subroutine scrollback_get_line(sb, offset, line, cols)
    type(scrollback_t), intent(in) :: sb
    integer, intent(in) :: offset
    type(cell_t), intent(out) :: line(:)
    integer, intent(in) :: cols
    integer :: idx, col, copy_cols

    ! Initialize output to spaces
    do col = 1, min(cols, size(line))
      line(col) = cell_t(32, default_fg, default_bg, 0)
    end do

    if (.not. sb%initialized) return
    if (offset < 0 .or. offset >= sb%count) return

    ! Calculate actual index in circular buffer
    ! head points to next write position, so most recent is at head-1
    idx = mod(sb%head - 1 - offset + sb%capacity, sb%capacity) + 1

    ! Copy line data
    copy_cols = min(cols, sb%cols, size(line))
    do col = 1, copy_cols
      line(col) = sb%lines(idx, col)
    end do
  end subroutine scrollback_get_line

  ! Clear scrollback buffer
  subroutine scrollback_clear(sb)
    type(scrollback_t), intent(inout) :: sb

    sb%count = 0
    sb%head = 0
  end subroutine scrollback_clear

  ! Get number of lines in scrollback
  function scrollback_count(sb) result(n)
    type(scrollback_t), intent(in) :: sb
    integer :: n

    n = sb%count
  end function scrollback_count

end module scrollback_mod
