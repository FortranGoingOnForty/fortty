module layout_mod
  use pane_mod
  implicit none
  private

  public :: layout_split_vertical, layout_split_horizontal
  public :: layout_remove_pane
  public :: layout_recalculate
  public :: layout_find_neighbor

  ! Direction constants for navigation
  integer, parameter, public :: DIR_LEFT = 1
  integer, parameter, public :: DIR_RIGHT = 2
  integer, parameter, public :: DIR_UP = 3
  integer, parameter, public :: DIR_DOWN = 4

  ! Split type constants
  integer, parameter, public :: SPLIT_NONE = 0
  integer, parameter, public :: SPLIT_VERTICAL = 1    ! Side-by-side (left|right)
  integer, parameter, public :: SPLIT_HORIZONTAL = 2  ! Stacked (top/bottom)

contains

  ! Split the active pane vertically (side-by-side)
  ! Returns the new pane index, or 0 on failure
  function layout_split_vertical(panes, pane_count, active_idx, rows, cols) result(new_idx)
    type(pane_t), intent(inout) :: panes(:)
    integer, intent(inout) :: pane_count
    integer, intent(in) :: active_idx
    integer, intent(in) :: rows, cols
    integer :: new_idx

    new_idx = 0
    if (pane_count >= MAX_PANES) return
    if (active_idx < 1 .or. active_idx > pane_count) return

    ! Create new pane
    pane_count = pane_count + 1
    new_idx = pane_count

    call pane_init(panes(new_idx), rows, cols)

    ! Mark the split relationship
    panes(active_idx)%split_type = SPLIT_VERTICAL
    panes(new_idx)%parent_idx = active_idx
    panes(new_idx)%split_ratio = 0.5  ! Right half
  end function layout_split_vertical

  ! Split the active pane horizontally (stacked top/bottom)
  ! Returns the new pane index, or 0 on failure
  function layout_split_horizontal(panes, pane_count, active_idx, rows, cols) result(new_idx)
    type(pane_t), intent(inout) :: panes(:)
    integer, intent(inout) :: pane_count
    integer, intent(in) :: active_idx
    integer, intent(in) :: rows, cols
    integer :: new_idx

    new_idx = 0
    if (pane_count >= MAX_PANES) return
    if (active_idx < 1 .or. active_idx > pane_count) return

    ! Create new pane
    pane_count = pane_count + 1
    new_idx = pane_count

    call pane_init(panes(new_idx), rows, cols)

    ! Mark the split relationship
    panes(active_idx)%split_type = SPLIT_HORIZONTAL
    panes(new_idx)%parent_idx = active_idx
    panes(new_idx)%split_ratio = 0.5  ! Bottom half
  end function layout_split_horizontal

  ! Remove a pane and update layout
  subroutine layout_remove_pane(panes, pane_count, idx)
    type(pane_t), intent(inout) :: panes(:)
    integer, intent(inout) :: pane_count
    integer, intent(in) :: idx
    integer :: i, parent_idx

    if (idx < 1 .or. idx > pane_count) return

    parent_idx = panes(idx)%parent_idx

    ! Destroy the pane
    call pane_destroy(panes(idx))

    ! Shift remaining panes down
    do i = idx, pane_count - 1
      panes(i) = panes(i + 1)
    end do

    pane_count = pane_count - 1

    ! Update parent references for shifted panes
    do i = 1, pane_count
      if (panes(i)%parent_idx > idx) then
        panes(i)%parent_idx = panes(i)%parent_idx - 1
      else if (panes(i)%parent_idx == idx) then
        ! Orphaned - reparent to the removed pane's parent
        panes(i)%parent_idx = parent_idx
      end if
    end do

    ! If we removed a pane that had children, clear the split type of sibling
    if (parent_idx > 0 .and. parent_idx <= pane_count) then
      panes(parent_idx)%split_type = SPLIT_NONE
    end if
  end subroutine layout_remove_pane

  ! Recalculate all pane viewports based on available space
  subroutine layout_recalculate(panes, pane_count, x, y, w, h, cell_w, cell_h)
    type(pane_t), intent(inout) :: panes(:)
    integer, intent(in) :: pane_count
    integer, intent(in) :: x, y, w, h
    integer, intent(in) :: cell_w, cell_h
    integer :: i
    integer, allocatable :: viewport(:,:)  ! (4, MAX_PANES) - x, y, w, h per pane
    logical, allocatable :: processed(:)
    integer :: px, py, pw, ph, split_pos

    if (pane_count < 1) return

    allocate(viewport(4, pane_count))
    allocate(processed(pane_count))
    processed = .false.

    ! Start by giving the first pane (root) the entire space
    viewport(1, 1) = x
    viewport(2, 1) = y
    viewport(3, 1) = w
    viewport(4, 1) = h
    processed(1) = .true.

    ! Process panes in order, calculating child viewports from parents
    do i = 2, pane_count
      if (panes(i)%parent_idx > 0 .and. panes(i)%parent_idx <= pane_count) then
        ! Get parent's viewport
        px = viewport(1, panes(i)%parent_idx)
        py = viewport(2, panes(i)%parent_idx)
        pw = viewport(3, panes(i)%parent_idx)
        ph = viewport(4, panes(i)%parent_idx)

        if (panes(panes(i)%parent_idx)%split_type == SPLIT_VERTICAL) then
          ! Parent was split vertically - child gets right half
          split_pos = pw / 2
          ! Shrink parent to left half
          viewport(3, panes(i)%parent_idx) = split_pos - 1
          ! Child gets right half
          viewport(1, i) = px + split_pos + 1
          viewport(2, i) = py
          viewport(3, i) = pw - split_pos - 2
          viewport(4, i) = ph
        else if (panes(panes(i)%parent_idx)%split_type == SPLIT_HORIZONTAL) then
          ! Parent was split horizontally - child gets bottom half
          split_pos = ph / 2
          ! Shrink parent to top half
          viewport(4, panes(i)%parent_idx) = split_pos - 1
          ! Child gets bottom half
          viewport(1, i) = px
          viewport(2, i) = py + split_pos + 1
          viewport(3, i) = pw
          viewport(4, i) = ph - split_pos - 2
        else
          ! No split type - shouldn't happen, give equal share
          viewport(1, i) = x
          viewport(2, i) = y
          viewport(3, i) = w
          viewport(4, i) = h
        end if
        processed(i) = .true.
      else
        ! Orphan pane - give it the full space (shouldn't happen normally)
        viewport(1, i) = x
        viewport(2, i) = y
        viewport(3, i) = w
        viewport(4, i) = h
        processed(i) = .true.
      end if
    end do

    ! Apply viewports to all panes
    do i = 1, pane_count
      call pane_set_viewport(panes(i), viewport(1, i), viewport(2, i), &
                             viewport(3, i), viewport(4, i), cell_w, cell_h)
    end do

    deallocate(viewport)
    deallocate(processed)
  end subroutine layout_recalculate

  ! Find the nearest pane in the given direction
  ! Returns 0 if no neighbor found
  function layout_find_neighbor(panes, pane_count, active_idx, direction) result(neighbor_idx)
    type(pane_t), intent(in) :: panes(:)
    integer, intent(in) :: pane_count
    integer, intent(in) :: active_idx
    integer, intent(in) :: direction
    integer :: neighbor_idx

    integer :: i
    integer :: active_cx, active_cy
    integer :: cx, cy
    integer :: best_idx, best_dist, dist, perp_dist

    neighbor_idx = 0
    if (pane_count < 2) return
    if (active_idx < 1 .or. active_idx > pane_count) return

    ! Get active pane's center point
    active_cx = panes(active_idx)%x + panes(active_idx)%width / 2
    active_cy = panes(active_idx)%y + panes(active_idx)%height / 2

    best_idx = 0
    best_dist = huge(best_dist)

    do i = 1, pane_count
      if (i == active_idx) cycle

      cx = panes(i)%x + panes(i)%width / 2
      cy = panes(i)%y + panes(i)%height / 2

      select case (direction)
        case (DIR_LEFT)
          if (cx < active_cx) then
            dist = active_cx - cx
            perp_dist = abs(cy - active_cy)
            ! Prefer panes more directly to the left
            dist = dist + perp_dist / 2
            if (dist < best_dist) then
              best_dist = dist
              best_idx = i
            end if
          end if

        case (DIR_RIGHT)
          if (cx > active_cx) then
            dist = cx - active_cx
            perp_dist = abs(cy - active_cy)
            dist = dist + perp_dist / 2
            if (dist < best_dist) then
              best_dist = dist
              best_idx = i
            end if
          end if

        case (DIR_UP)
          if (cy < active_cy) then
            dist = active_cy - cy
            perp_dist = abs(cx - active_cx)
            dist = dist + perp_dist / 2
            if (dist < best_dist) then
              best_dist = dist
              best_idx = i
            end if
          end if

        case (DIR_DOWN)
          if (cy > active_cy) then
            dist = cy - active_cy
            perp_dist = abs(cx - active_cx)
            dist = dist + perp_dist / 2
            if (dist < best_dist) then
              best_dist = dist
              best_idx = i
            end if
          end if
      end select
    end do

    neighbor_idx = best_idx
  end function layout_find_neighbor

end module layout_mod
