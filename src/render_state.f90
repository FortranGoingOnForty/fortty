! Render state module - holds state for the render callback
! This allows do_render to be a module procedure instead of an internal
! procedure, avoiding the need for trampolines and executable stack.
module render_state_mod
  use window_mod
  use gl_bindings
  use renderer_mod
  use terminal_mod
  use screen_mod
  use cell_mod
  use config_mod
  use cursor_mod, only: CURSOR_BLOCK, CURSOR_UNDERLINE, CURSOR_BAR
  use tab_manager_mod
  use tab_bar_mod
  use pane_mod
  use selection_mod, only: selection_t, selection_contains
  implicit none
  private

  public :: render_state_init
  public :: render_state_update_blink
  public :: do_render

  ! Pointers to main program state
  type(window_t), pointer, save :: rs_win => null()
  type(renderer_t), pointer, save :: rs_ren => null()
  type(tab_manager_t), pointer, save :: rs_tab_mgr => null()
  type(config_t), pointer, save :: rs_cfg => null()

  ! Scalar state (copied, not pointed)
  integer, save :: rs_prev_width = 0
  integer, save :: rs_prev_height = 0
  integer, save :: rs_win_width = 0
  integer, save :: rs_win_height = 0
  integer, save :: rs_cell_width = 10
  integer, save :: rs_cell_height = 18
  integer, save :: rs_ascender = 14
  logical, save :: rs_cursor_blink_visible = .true.

  ! Scrollback line buffer
  type(cell_t), allocatable, save :: rs_sb_line(:)

contains

  ! Initialize render state with pointers to main program data
  subroutine render_state_init(win, ren, tab_mgr, cfg, &
                               cell_width, cell_height, ascender, &
                               win_width, win_height)
    type(window_t), target, intent(in) :: win
    type(renderer_t), target, intent(in) :: ren
    type(tab_manager_t), target, intent(in) :: tab_mgr
    type(config_t), target, intent(in) :: cfg
    integer, intent(in) :: cell_width, cell_height, ascender
    integer, intent(in) :: win_width, win_height

    rs_win => win
    rs_ren => ren
    rs_tab_mgr => tab_mgr
    rs_cfg => cfg
    rs_cell_width = cell_width
    rs_cell_height = cell_height
    rs_ascender = ascender
    rs_win_width = win_width
    rs_win_height = win_height
    rs_prev_width = win_width
    rs_prev_height = win_height
  end subroutine render_state_init

  ! Update render state with current values
  subroutine render_state_update_blink(cursor_blink_visible)
    logical, intent(in) :: cursor_blink_visible
    rs_cursor_blink_visible = cursor_blink_visible
  end subroutine render_state_update_blink

  ! Main render subroutine - now a module procedure, not internal
  subroutine do_render()
    integer :: render_width, render_height
    integer :: pane_idx, tab_idx, tab_bar_height
    integer :: scissor_x, scissor_y, scissor_w, scissor_h
    integer :: hover_tab
    logical :: hover_close
    real :: dim_factor, pane_x_offset, pane_y_offset
    real :: bg_r, bg_g, bg_b
    type(pane_t), pointer :: cur_pane
    type(terminal_t), pointer :: pane_term
    type(screen_t), pointer :: pane_scr
    type(selection_t) :: sel
    type(cell_t) :: cell
    integer :: row, col, scroll_offset, sb_offset, screen_row
    real :: x, y, r, g, b

    ! Guard against uninitialized state
    if (.not. associated(rs_win)) return
    if (.not. associated(rs_ren)) return
    if (.not. associated(rs_tab_mgr)) return
    if (.not. associated(rs_cfg)) return

    ! Get current window size and update projection if needed
    call window_get_size(rs_win, render_width, render_height)
    if (render_width /= rs_prev_width .or. render_height /= rs_prev_height) then
      rs_prev_width = render_width
      rs_prev_height = render_height
      rs_win_width = render_width
      rs_win_height = render_height
      call renderer_set_projection(rs_ren, rs_win_width, rs_win_height)
    end if

    ! Clear screen with background color and opacity from config
    bg_r = real(rs_cfg%bg_color%r) / 255.0
    bg_g = real(rs_cfg%bg_color%g) / 255.0
    bg_b = real(rs_cfg%bg_color%b) / 255.0
    call glClearColor(bg_r, bg_g, bg_b, rs_cfg%window_opacity)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Render terminal buffer
    call renderer_begin(rs_ren)

    ! Get tab hover state for close button highlighting
    call window_get_tab_hover(hover_tab, hover_close)

    ! Render tab bar at top
    call tab_bar_render(rs_ren, rs_tab_mgr, rs_win_width, rs_tab_mgr%bar_height, &
                        rs_cell_width, rs_ascender, hover_tab, hover_close)

    ! Calculate effective tab bar height (hidden when only 1 tab)
    if (rs_tab_mgr%count > 1) then
      tab_bar_height = rs_tab_mgr%bar_height
    else
      tab_bar_height = 0
    end if

    ! Update window module with tab bar info for click detection
    call window_set_tab_bar_info(tab_bar_height, rs_tab_mgr%count, rs_win_width)

    ! Guard against no active tab
    if (rs_tab_mgr%active_index < 1 .or. rs_tab_mgr%active_index > rs_tab_mgr%count) then
      call renderer_flush(rs_ren)
      call window_swap_buffers(rs_win)
      return
    end if

    tab_idx = rs_tab_mgr%active_index

    ! Get current selection for highlighting (only applies to active pane)
    sel = window_get_selection()

    ! Render each pane in the active tab
    do pane_idx = 1, rs_tab_mgr%tabs(tab_idx)%pane_count
      cur_pane => rs_tab_mgr%tabs(tab_idx)%panes(pane_idx)
      pane_term => cur_pane%term
      pane_scr => terminal_active_screen(pane_term)

      ! Determine dimming factor (inactive panes are dimmed)
      if (cur_pane%active) then
        dim_factor = 1.0
      else
        dim_factor = 0.6
      end if

      ! Calculate pane offset for rendering
      pane_x_offset = real(cur_pane%x)
      pane_y_offset = real(cur_pane%y)

      ! Enable scissor test for this pane's viewport
      ! OpenGL uses bottom-left origin, so convert from top-left
      scissor_x = cur_pane%x
      scissor_y = rs_win_height - cur_pane%y - cur_pane%height
      scissor_w = cur_pane%width
      scissor_h = cur_pane%height
      call glEnable(GL_SCISSOR_TEST)
      call glScissor(scissor_x, scissor_y, scissor_w, scissor_h)

      scroll_offset = terminal_get_scroll_offset(pane_term)

      ! Allocate/resize scrollback line buffer if needed
      if (.not. allocated(rs_sb_line)) then
        allocate(rs_sb_line(cur_pane%cols))
      else if (size(rs_sb_line) /= cur_pane%cols) then
        deallocate(rs_sb_line)
        allocate(rs_sb_line(cur_pane%cols))
      end if

      do row = 1, pane_scr%rows
        ! Cell top-left y coordinate relative to pane
        y = real(row - 1) * rs_cell_height + pane_y_offset

        ! Determine if this row shows scrollback or screen content
        sb_offset = scroll_offset - row + 1

        if (sb_offset > 0 .and. sb_offset <= terminal_get_scrollback_count(pane_term)) then
          ! This row shows scrollback content
          call terminal_get_scrollback_line(pane_term, sb_offset - 1, rs_sb_line, cur_pane%cols)
          do col = 1, min(cur_pane%cols, pane_scr%cols)
            cell = rs_sb_line(col)

            ! Skip continuation cells (2nd half of wide chars)
            if (cell%is_continuation) cycle

            x = real(col - 1) * rs_cell_width + pane_x_offset

            ! Draw selection background if selected (only for active pane)
            if (cur_pane%active .and. selection_contains(sel, row, col)) then
              call renderer_draw_rect(rs_ren, x, y, real(rs_cell_width), real(rs_cell_height), &
                                      0.3, 0.3, 0.6, 1.0)
            end if

            ! Only render cells with actual text content
            if (cell%codepoint /= 32 .and. cell%codepoint /= 0) then
              r = real(cell%fg%r) / 255.0 * dim_factor
              g = real(cell%fg%g) / 255.0 * dim_factor
              b = real(cell%fg%b) / 255.0 * dim_factor
              call renderer_draw_char(rs_ren, x, y + real(rs_ascender), cell%codepoint, r, g, b, 1.0)
            end if
          end do
        else
          ! This row shows screen content
          screen_row = row - scroll_offset
          if (screen_row >= 1 .and. screen_row <= pane_scr%rows) then
            do col = 1, pane_scr%cols
              cell = screen_get_cell(pane_scr, screen_row, col)

              ! Skip continuation cells (2nd half of wide chars)
              if (cell%is_continuation) cycle

              x = real(col - 1) * rs_cell_width + pane_x_offset

              ! Draw selection background if selected (only for active pane)
              if (cur_pane%active .and. selection_contains(sel, row, col)) then
                call renderer_draw_rect(rs_ren, x, y, real(rs_cell_width), real(rs_cell_height), &
                                        0.3, 0.3, 0.6, 1.0)
              end if

              ! Only render cells with actual text content
              if (cell%codepoint /= 32 .and. cell%codepoint /= 0) then
                r = real(cell%fg%r) / 255.0 * dim_factor
                g = real(cell%fg%g) / 255.0 * dim_factor
                b = real(cell%fg%b) / 255.0 * dim_factor
                call renderer_draw_char(rs_ren, x, y + real(rs_ascender), cell%codepoint, r, g, b, 1.0)
              end if
            end do
          end if
        end if
      end do

      ! Draw cursor if visible (only for active pane and when not scrolled back)
      if (cur_pane%active .and. pane_term%cursor%visible .and. scroll_offset == 0) then
        ! Check blink state - only hide cursor if blink is enabled and in off phase
        if (.not. pane_term%cursor%blink .or. rs_cursor_blink_visible) then
          x = real(pane_term%cursor%col - 1) * rs_cell_width + pane_x_offset
          y = real(pane_term%cursor%row - 1) * rs_cell_height + pane_y_offset

          select case (pane_term%cursor%style)
            case (CURSOR_BLOCK)
              call renderer_draw_rect(rs_ren, x, y, real(rs_cell_width), real(rs_cell_height), &
                                      0.7, 0.7, 0.7, 0.8)
            case (CURSOR_UNDERLINE)
              call renderer_draw_rect(rs_ren, x, y + real(rs_ascender), &
                                      real(rs_cell_width), 2.0, 0.7, 0.7, 0.7, 1.0)
            case (CURSOR_BAR)
              call renderer_draw_rect(rs_ren, x, y, 2.0, real(rs_cell_height), 0.7, 0.7, 0.7, 1.0)
            case default
              call renderer_draw_rect(rs_ren, x, y, real(rs_cell_width), real(rs_cell_height), &
                                      0.7, 0.7, 0.7, 0.8)
          end select
        end if
      end if

      call glDisable(GL_SCISSOR_TEST)
    end do

    call renderer_flush(rs_ren)

    ! Swap buffers
    call window_swap_buffers(rs_win)
  end subroutine do_render

end module render_state_mod
