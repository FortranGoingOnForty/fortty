module tab_manager_mod
  use terminal_mod
  use pty_mod
  use parser_mod
  use pane_mod
  use layout_mod
  implicit none
  private

  public :: tab_t, tab_manager_t
  public :: tab_manager_init, tab_manager_destroy
  public :: tab_manager_add, tab_manager_close
  public :: tab_manager_switch, tab_manager_next, tab_manager_prev
  public :: tab_manager_has_tabs, tab_manager_get_active_term, tab_manager_get_active_pty
  public :: tab_manager_get_active_pane
  public :: tab_manager_split_pane_v, tab_manager_split_pane_h
  public :: tab_manager_close_pane
  public :: tab_manager_navigate_pane
  public :: tab_manager_recalculate_layout

  ! Maximum number of tabs supported
  integer, parameter :: MAX_TABS = 32

  type :: tab_t
    type(pane_t), allocatable :: panes(:)
    integer :: active_pane = 1
    integer :: pane_count = 0
    character(len=256) :: title = ''
    logical :: active = .false.
  end type tab_t

  type :: tab_manager_t
    type(tab_t), allocatable :: tabs(:)
    integer :: active_index = 0
    integer :: count = 0
    integer :: bar_height = 28  ! pixels
    integer :: term_rows = 24
    integer :: term_cols = 80
    ! Stored for layout recalculation
    integer :: cell_width = 10
    integer :: cell_height = 18
  end type tab_manager_t

contains

  ! Initialize tab manager with one tab
  subroutine tab_manager_init(mgr, rows, cols)
    type(tab_manager_t), intent(inout) :: mgr
    integer, intent(in) :: rows, cols

    mgr%term_rows = rows
    mgr%term_cols = cols
    mgr%count = 0
    mgr%active_index = 0

    allocate(mgr%tabs(MAX_TABS))

    ! Create the first tab
    call tab_manager_add(mgr)
  end subroutine tab_manager_init

  ! Destroy tab manager and all tabs
  subroutine tab_manager_destroy(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: i, j

    do i = 1, mgr%count
      do j = 1, mgr%tabs(i)%pane_count
        call pane_destroy(mgr%tabs(i)%panes(j))
      end do
      if (allocated(mgr%tabs(i)%panes)) deallocate(mgr%tabs(i)%panes)
    end do

    if (allocated(mgr%tabs)) deallocate(mgr%tabs)
    mgr%count = 0
    mgr%active_index = 0
  end subroutine tab_manager_destroy

  ! Add a new tab with a single pane
  subroutine tab_manager_add(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: idx
    character(len=32) :: title_buf

    if (mgr%count >= MAX_TABS) then
      print *, "Warning: Maximum number of tabs reached"
      return
    end if

    idx = mgr%count + 1

    ! Allocate panes array for this tab
    allocate(mgr%tabs(idx)%panes(MAX_PANES))
    mgr%tabs(idx)%pane_count = 0
    mgr%tabs(idx)%active_pane = 1

    ! Create the first pane in this tab
    mgr%tabs(idx)%pane_count = 1
    call pane_init(mgr%tabs(idx)%panes(1), mgr%term_rows, mgr%term_cols)

    if (.not. mgr%tabs(idx)%panes(1)%pty%active) then
      print *, "Error: Could not open PTY for new tab"
      call pane_destroy(mgr%tabs(idx)%panes(1))
      deallocate(mgr%tabs(idx)%panes)
      return
    end if

    ! Mark pane as active
    mgr%tabs(idx)%panes(1)%active = .true.

    ! Set default title
    write(title_buf, '(A,I0)') 'Tab ', idx
    mgr%tabs(idx)%title = trim(title_buf)

    ! Mark as active and update state
    mgr%tabs(idx)%active = .true.
    mgr%count = idx

    ! Switch to the new tab
    call tab_manager_switch(mgr, idx)
  end subroutine tab_manager_add

  ! Close a tab by index (closes all panes in the tab)
  subroutine tab_manager_close(mgr, idx)
    type(tab_manager_t), intent(inout) :: mgr
    integer, intent(in) :: idx
    integer :: i, j

    if (idx < 1 .or. idx > mgr%count) return

    ! Destroy all panes in this tab
    do j = 1, mgr%tabs(idx)%pane_count
      call pane_destroy(mgr%tabs(idx)%panes(j))
    end do
    if (allocated(mgr%tabs(idx)%panes)) deallocate(mgr%tabs(idx)%panes)

    ! Shift remaining tabs down
    do i = idx, mgr%count - 1
      mgr%tabs(i) = mgr%tabs(i + 1)
    end do

    mgr%count = mgr%count - 1

    ! Update active index if needed
    if (mgr%count == 0) then
      mgr%active_index = 0
    else if (mgr%active_index > mgr%count) then
      mgr%active_index = mgr%count
    else if (mgr%active_index >= idx .and. mgr%active_index > 1) then
      mgr%active_index = mgr%active_index - 1
    end if

    ! Update active flags
    do i = 1, mgr%count
      mgr%tabs(i)%active = (i == mgr%active_index)
    end do
  end subroutine tab_manager_close

  ! Split the active pane vertically (side-by-side)
  subroutine tab_manager_split_pane_v(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: tab_idx, pane_idx, new_idx

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index
    pane_idx = mgr%tabs(tab_idx)%active_pane

    new_idx = layout_split_vertical(mgr%tabs(tab_idx)%panes, &
                                    mgr%tabs(tab_idx)%pane_count, &
                                    pane_idx, &
                                    mgr%term_rows, mgr%term_cols)

    if (new_idx > 0) then
      ! Switch focus to the new pane
      mgr%tabs(tab_idx)%panes(pane_idx)%active = .false.
      mgr%tabs(tab_idx)%panes(new_idx)%active = .true.
      mgr%tabs(tab_idx)%active_pane = new_idx
    end if
  end subroutine tab_manager_split_pane_v

  ! Split the active pane horizontally (stacked)
  subroutine tab_manager_split_pane_h(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: tab_idx, pane_idx, new_idx

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index
    pane_idx = mgr%tabs(tab_idx)%active_pane

    new_idx = layout_split_horizontal(mgr%tabs(tab_idx)%panes, &
                                      mgr%tabs(tab_idx)%pane_count, &
                                      pane_idx, &
                                      mgr%term_rows, mgr%term_cols)

    if (new_idx > 0) then
      ! Switch focus to the new pane
      mgr%tabs(tab_idx)%panes(pane_idx)%active = .false.
      mgr%tabs(tab_idx)%panes(new_idx)%active = .true.
      mgr%tabs(tab_idx)%active_pane = new_idx
    end if
  end subroutine tab_manager_split_pane_h

  ! Close the active pane in the active tab
  ! Returns .true. if a pane was closed, .false. if tab should be closed
  subroutine tab_manager_close_pane(mgr, should_close_tab)
    type(tab_manager_t), intent(inout) :: mgr
    logical, intent(out) :: should_close_tab
    integer :: tab_idx, pane_idx, new_active

    should_close_tab = .false.

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index

    ! If only one pane, signal to close the tab instead
    if (mgr%tabs(tab_idx)%pane_count <= 1) then
      should_close_tab = .true.
      return
    end if

    pane_idx = mgr%tabs(tab_idx)%active_pane

    ! Determine new active pane before removal
    if (pane_idx > 1) then
      new_active = pane_idx - 1
    else
      new_active = 1
    end if

    ! Remove the pane
    call layout_remove_pane(mgr%tabs(tab_idx)%panes, &
                            mgr%tabs(tab_idx)%pane_count, &
                            pane_idx)

    ! Update active pane
    if (new_active > mgr%tabs(tab_idx)%pane_count) then
      new_active = mgr%tabs(tab_idx)%pane_count
    end if
    mgr%tabs(tab_idx)%active_pane = new_active

    ! Update active flags
    do pane_idx = 1, mgr%tabs(tab_idx)%pane_count
      mgr%tabs(tab_idx)%panes(pane_idx)%active = (pane_idx == new_active)
    end do
  end subroutine tab_manager_close_pane

  ! Navigate to a pane in the given direction
  subroutine tab_manager_navigate_pane(mgr, direction)
    type(tab_manager_t), intent(inout) :: mgr
    integer, intent(in) :: direction
    integer :: tab_idx, neighbor_idx, old_active

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index

    if (mgr%tabs(tab_idx)%pane_count < 2) return

    neighbor_idx = layout_find_neighbor(mgr%tabs(tab_idx)%panes, &
                                        mgr%tabs(tab_idx)%pane_count, &
                                        mgr%tabs(tab_idx)%active_pane, &
                                        direction)

    if (neighbor_idx > 0) then
      old_active = mgr%tabs(tab_idx)%active_pane
      mgr%tabs(tab_idx)%panes(old_active)%active = .false.
      mgr%tabs(tab_idx)%panes(neighbor_idx)%active = .true.
      mgr%tabs(tab_idx)%active_pane = neighbor_idx
    end if
  end subroutine tab_manager_navigate_pane

  ! Recalculate layout for all panes in the active tab
  subroutine tab_manager_recalculate_layout(mgr, x, y, w, h, cell_w, cell_h)
    type(tab_manager_t), intent(inout) :: mgr
    integer, intent(in) :: x, y, w, h
    integer, intent(in) :: cell_w, cell_h
    integer :: tab_idx

    ! Store cell dimensions for later use
    mgr%cell_width = cell_w
    mgr%cell_height = cell_h

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index

    call layout_recalculate(mgr%tabs(tab_idx)%panes, &
                            mgr%tabs(tab_idx)%pane_count, &
                            x, y, w, h, cell_w, cell_h)
  end subroutine tab_manager_recalculate_layout

  ! Switch to a specific tab
  subroutine tab_manager_switch(mgr, idx)
    type(tab_manager_t), intent(inout) :: mgr
    integer, intent(in) :: idx
    integer :: i

    if (idx < 1 .or. idx > mgr%count) return

    ! Update active flags
    do i = 1, mgr%count
      mgr%tabs(i)%active = (i == idx)
    end do

    mgr%active_index = idx
  end subroutine tab_manager_switch

  ! Switch to next tab (wrapping)
  subroutine tab_manager_next(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: next_idx

    if (mgr%count <= 1) return

    next_idx = mgr%active_index + 1
    if (next_idx > mgr%count) next_idx = 1

    call tab_manager_switch(mgr, next_idx)
  end subroutine tab_manager_next

  ! Switch to previous tab (wrapping)
  subroutine tab_manager_prev(mgr)
    type(tab_manager_t), intent(inout) :: mgr
    integer :: prev_idx

    if (mgr%count <= 1) return

    prev_idx = mgr%active_index - 1
    if (prev_idx < 1) prev_idx = mgr%count

    call tab_manager_switch(mgr, prev_idx)
  end subroutine tab_manager_prev

  ! Check if there are any tabs
  function tab_manager_has_tabs(mgr) result(has)
    type(tab_manager_t), intent(in) :: mgr
    logical :: has

    has = mgr%count > 0
  end function tab_manager_has_tabs

  ! Get pointer to active pane
  function tab_manager_get_active_pane(mgr) result(pane_ptr)
    type(tab_manager_t), intent(in), target :: mgr
    type(pane_t), pointer :: pane_ptr
    integer :: tab_idx, pane_idx

    pane_ptr => null()

    if (mgr%active_index < 1 .or. mgr%active_index > mgr%count) return
    tab_idx = mgr%active_index

    pane_idx = mgr%tabs(tab_idx)%active_pane
    if (pane_idx < 1 .or. pane_idx > mgr%tabs(tab_idx)%pane_count) return

    pane_ptr => mgr%tabs(tab_idx)%panes(pane_idx)
  end function tab_manager_get_active_pane

  ! Get pointer to active terminal (through active pane)
  function tab_manager_get_active_term(mgr) result(term_ptr)
    type(tab_manager_t), intent(in), target :: mgr
    type(terminal_t), pointer :: term_ptr
    type(pane_t), pointer :: pane_ptr

    term_ptr => null()
    pane_ptr => tab_manager_get_active_pane(mgr)
    if (associated(pane_ptr)) then
      term_ptr => pane_ptr%term
    end if
  end function tab_manager_get_active_term

  ! Get pointer to active PTY (through active pane)
  function tab_manager_get_active_pty(mgr) result(pty_ptr)
    type(tab_manager_t), intent(in), target :: mgr
    type(pty_t), pointer :: pty_ptr
    type(pane_t), pointer :: pane_ptr

    pty_ptr => null()
    pane_ptr => tab_manager_get_active_pane(mgr)
    if (associated(pane_ptr)) then
      pty_ptr => pane_ptr%pty
    end if
  end function tab_manager_get_active_pty

end module tab_manager_mod
