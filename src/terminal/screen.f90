module screen_mod
  use cell_mod
  implicit none
  private

  public :: screen_t
  public :: screen_init, screen_destroy, screen_resize
  public :: screen_clear, screen_clear_line, screen_clear_region
  public :: screen_set_cell, screen_get_cell
  public :: screen_mark_dirty, screen_mark_all_dirty
  public :: screen_clear_dirty, screen_is_dirty

  type :: screen_t
    integer :: rows = 0
    integer :: cols = 0
    type(cell_t), allocatable :: cells(:,:)  ! (row, col)
    logical, allocatable :: dirty(:)          ! Per-line dirty flags
  end type screen_t

contains

  ! Initialize screen buffer
  subroutine screen_init(scr, rows, cols)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: rows, cols
    integer :: r, c

    scr%rows = rows
    scr%cols = cols

    allocate(scr%cells(rows, cols))
    allocate(scr%dirty(rows))

    ! Initialize all cells to spaces with default colors
    do r = 1, rows
      do c = 1, cols
        scr%cells(r, c) = cell_t(32, default_fg, default_bg, 0)
      end do
    end do

    ! Mark all lines as dirty initially
    scr%dirty = .true.
  end subroutine screen_init

  ! Destroy screen buffer
  subroutine screen_destroy(scr)
    type(screen_t), intent(inout) :: scr

    if (allocated(scr%cells)) deallocate(scr%cells)
    if (allocated(scr%dirty)) deallocate(scr%dirty)
    scr%rows = 0
    scr%cols = 0
  end subroutine screen_destroy

  ! Resize screen buffer (preserves content where possible)
  subroutine screen_resize(scr, new_rows, new_cols)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: new_rows, new_cols
    type(cell_t), allocatable :: new_cells(:,:)
    logical, allocatable :: new_dirty(:)
    integer :: min_rows, min_cols, r, c

    if (new_rows == scr%rows .and. new_cols == scr%cols) return

    allocate(new_cells(new_rows, new_cols))
    allocate(new_dirty(new_rows))

    ! Initialize new buffer with spaces
    do r = 1, new_rows
      do c = 1, new_cols
        new_cells(r, c) = cell_t(32, default_fg, default_bg, 0)
      end do
    end do

    ! Copy existing content
    min_rows = min(scr%rows, new_rows)
    min_cols = min(scr%cols, new_cols)

    do r = 1, min_rows
      do c = 1, min_cols
        new_cells(r, c) = scr%cells(r, c)
      end do
    end do

    ! Replace buffers
    call move_alloc(new_cells, scr%cells)
    call move_alloc(new_dirty, scr%dirty)

    scr%rows = new_rows
    scr%cols = new_cols

    ! Mark all lines dirty after resize
    scr%dirty = .true.
  end subroutine screen_resize

  ! Clear entire screen
  subroutine screen_clear(scr)
    type(screen_t), intent(inout) :: scr
    integer :: r, c

    do r = 1, scr%rows
      do c = 1, scr%cols
        scr%cells(r, c) = cell_t(32, default_fg, default_bg, 0)
      end do
      scr%dirty(r) = .true.
    end do
  end subroutine screen_clear

  ! Clear a single line
  subroutine screen_clear_line(scr, row)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: row
    integer :: c

    if (row < 1 .or. row > scr%rows) return

    do c = 1, scr%cols
      scr%cells(row, c) = cell_t(32, default_fg, default_bg, 0)
    end do
    scr%dirty(row) = .true.
  end subroutine screen_clear_line

  ! Clear a region of the screen
  subroutine screen_clear_region(scr, r1, c1, r2, c2)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: r1, c1, r2, c2
    integer :: r, c, row_start, row_end, col_start, col_end

    row_start = max(1, min(r1, r2))
    row_end = min(scr%rows, max(r1, r2))
    col_start = max(1, min(c1, c2))
    col_end = min(scr%cols, max(c1, c2))

    do r = row_start, row_end
      do c = col_start, col_end
        scr%cells(r, c) = cell_t(32, default_fg, default_bg, 0)
      end do
      scr%dirty(r) = .true.
    end do
  end subroutine screen_clear_region

  ! Set a cell at given position
  subroutine screen_set_cell(scr, row, col, cell)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: row, col
    type(cell_t), intent(in) :: cell

    if (row < 1 .or. row > scr%rows) return
    if (col < 1 .or. col > scr%cols) return

    scr%cells(row, col) = cell
    scr%dirty(row) = .true.
  end subroutine screen_set_cell

  ! Get a cell at given position
  function screen_get_cell(scr, row, col) result(cell)
    type(screen_t), intent(in) :: scr
    integer, intent(in) :: row, col
    type(cell_t) :: cell

    if (row < 1 .or. row > scr%rows .or. col < 1 .or. col > scr%cols) then
      cell = cell_t(32, default_fg, default_bg, 0)
      return
    end if

    cell = scr%cells(row, col)
  end function screen_get_cell

  ! Mark a line as dirty
  subroutine screen_mark_dirty(scr, row)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: row

    if (row >= 1 .and. row <= scr%rows) then
      scr%dirty(row) = .true.
    end if
  end subroutine screen_mark_dirty

  ! Mark all lines as dirty
  subroutine screen_mark_all_dirty(scr)
    type(screen_t), intent(inout) :: scr

    scr%dirty = .true.
  end subroutine screen_mark_all_dirty

  ! Clear dirty flag for a line
  subroutine screen_clear_dirty(scr, row)
    type(screen_t), intent(inout) :: scr
    integer, intent(in) :: row

    if (row >= 1 .and. row <= scr%rows) then
      scr%dirty(row) = .false.
    end if
  end subroutine screen_clear_dirty

  ! Check if a line is dirty
  function screen_is_dirty(scr, row) result(is_dirty)
    type(screen_t), intent(in) :: scr
    integer, intent(in) :: row
    logical :: is_dirty

    if (row < 1 .or. row > scr%rows) then
      is_dirty = .false.
      return
    end if

    is_dirty = scr%dirty(row)
  end function screen_is_dirty

end module screen_mod
