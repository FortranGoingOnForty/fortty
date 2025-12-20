module selection_mod
  use cell_mod
  use screen_mod
  implicit none
  private

  public :: selection_t
  public :: selection_start, selection_update, selection_end
  public :: selection_clear, selection_contains
  public :: selection_is_active, selection_get_bounds
  public :: selection_normalize

  type :: selection_t
    logical :: active = .false.
    logical :: selecting = .false.  ! Mouse button held
    integer :: start_row = 0
    integer :: start_col = 0
    integer :: end_row = 0
    integer :: end_col = 0
  end type selection_t

contains

  subroutine selection_start(sel, row, col)
    type(selection_t), intent(inout) :: sel
    integer, intent(in) :: row, col

    sel%start_row = row
    sel%start_col = col
    sel%end_row = row
    sel%end_col = col
    sel%selecting = .true.
    sel%active = .false.  ! Not active until mouse moves
  end subroutine selection_start

  subroutine selection_update(sel, row, col)
    type(selection_t), intent(inout) :: sel
    integer, intent(in) :: row, col

    if (.not. sel%selecting) return

    sel%end_row = row
    sel%end_col = col

    ! Mark as active if selection spans at least one cell
    if (sel%start_row /= sel%end_row .or. sel%start_col /= sel%end_col) then
      sel%active = .true.
    end if
  end subroutine selection_update

  subroutine selection_end(sel)
    type(selection_t), intent(inout) :: sel

    sel%selecting = .false.
    ! Keep active if we have a valid selection
  end subroutine selection_end

  subroutine selection_clear(sel)
    type(selection_t), intent(inout) :: sel

    sel%active = .false.
    sel%selecting = .false.
    sel%start_row = 0
    sel%start_col = 0
    sel%end_row = 0
    sel%end_col = 0
  end subroutine selection_clear

  function selection_is_active(sel) result(active)
    type(selection_t), intent(in) :: sel
    logical :: active

    active = sel%active
  end function selection_is_active

  subroutine selection_normalize(sel, r1, c1, r2, c2)
    type(selection_t), intent(in) :: sel
    integer, intent(out) :: r1, c1, r2, c2

    ! Return normalized bounds (r1,c1) <= (r2,c2) in reading order
    if (sel%start_row < sel%end_row .or. &
        (sel%start_row == sel%end_row .and. sel%start_col <= sel%end_col)) then
      r1 = sel%start_row
      c1 = sel%start_col
      r2 = sel%end_row
      c2 = sel%end_col
    else
      r1 = sel%end_row
      c1 = sel%end_col
      r2 = sel%start_row
      c2 = sel%start_col
    end if
  end subroutine selection_normalize

  subroutine selection_get_bounds(sel, r1, c1, r2, c2)
    type(selection_t), intent(in) :: sel
    integer, intent(out) :: r1, c1, r2, c2

    call selection_normalize(sel, r1, c1, r2, c2)
  end subroutine selection_get_bounds

  function selection_contains(sel, row, col) result(contains)
    type(selection_t), intent(in) :: sel
    integer, intent(in) :: row, col
    logical :: contains
    integer :: r1, c1, r2, c2

    contains = .false.
    if (.not. sel%active) return

    call selection_normalize(sel, r1, c1, r2, c2)

    ! Check if (row, col) is within selection
    if (row < r1 .or. row > r2) return

    if (r1 == r2) then
      ! Single line selection
      contains = (col >= c1 .and. col <= c2)
    else if (row == r1) then
      ! First line of multi-line selection
      contains = (col >= c1)
    else if (row == r2) then
      ! Last line of multi-line selection
      contains = (col <= c2)
    else
      ! Middle lines are fully selected
      contains = .true.
    end if
  end function selection_contains

end module selection_mod
