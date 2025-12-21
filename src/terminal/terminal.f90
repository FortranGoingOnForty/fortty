module terminal_mod
  use cell_mod
  use screen_mod
  use cursor_mod
  use scrollback_mod
  use wcwidth_mod, only: codepoint_width
  implicit none
  private

  public :: terminal_t
  public :: terminal_init, terminal_destroy, terminal_resize, terminal_reset
  public :: terminal_put_char, terminal_newline, terminal_carriage_return
  public :: terminal_tab, terminal_backspace
  public :: terminal_scroll_up, terminal_scroll_down
  public :: terminal_erase_display, terminal_erase_line
  public :: terminal_cursor_move, terminal_cursor_up, terminal_cursor_down
  public :: terminal_cursor_forward, terminal_cursor_backward
  public :: terminal_save_cursor, terminal_restore_cursor
  public :: terminal_set_scroll_region
  public :: terminal_insert_lines, terminal_delete_lines
  public :: terminal_insert_chars, terminal_delete_chars
  public :: terminal_index, terminal_reverse_index
  public :: terminal_switch_screen, terminal_active_screen
  public :: terminal_scroll_view, terminal_get_scroll_offset, terminal_reset_scroll_view
  public :: terminal_get_scrollback_count, terminal_get_scrollback_line
  public :: terminal_queue_response, terminal_get_response, terminal_has_response
  public :: terminal_set_title, terminal_get_title, terminal_has_title_changed

  integer, parameter :: RESPONSE_BUFFER_SIZE = 256

  type :: terminal_t
    type(screen_t) :: screen          ! Primary screen buffer
    type(screen_t) :: alt_screen      ! Alternate screen buffer
    logical :: using_alt = .false.    ! Which screen is active

    type(cursor_t) :: cursor          ! Current cursor state
    type(cursor_t) :: saved_cursor    ! Saved cursor (for DECSC/DECRC)

    integer :: rows = 24              ! Terminal rows
    integer :: cols = 80              ! Terminal columns
    integer :: scroll_top = 1         ! Scroll region top
    integer :: scroll_bottom = 24     ! Scroll region bottom

    ! Scrollback buffer (for primary screen only)
    type(scrollback_t) :: scrollback
    integer :: scroll_offset = 0      ! View offset into scrollback (0 = live view)

    ! Mode flags
    logical :: mode_autowrap = .true.   ! Auto-wrap at end of line
    logical :: mode_origin = .false.    ! Origin mode (cursor relative to scroll region)
    logical :: mode_insert = .false.    ! Insert mode
    logical :: pending_wrap = .false.   ! Deferred wrap: cursor at EOL, wrap on next char

    ! Tab stops
    logical, allocatable :: tabstops(:)

    ! Response buffer for escape sequence replies (e.g., DA1)
    character(len=RESPONSE_BUFFER_SIZE) :: response = ''
    integer :: response_len = 0

    ! Window title (set via OSC 0/1/2)
    character(len=256) :: title = 'fortty'
    logical :: title_changed = .false.
  end type terminal_t

contains

  ! Initialize terminal
  subroutine terminal_init(term, rows, cols)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: rows, cols
    integer :: i

    term%rows = rows
    term%cols = cols
    term%scroll_top = 1
    term%scroll_bottom = rows
    term%scroll_offset = 0

    ! Initialize both screen buffers
    call screen_init(term%screen, rows, cols)
    call screen_init(term%alt_screen, rows, cols)

    ! Initialize scrollback buffer
    call scrollback_init(term%scrollback, cols)

    ! Initialize cursor with default colors
    term%cursor%row = 1
    term%cursor%col = 1
    term%cursor%fg = default_fg
    term%cursor%bg = default_bg
    term%cursor%attrs = 0

    ! Initialize tab stops every 8 columns
    allocate(term%tabstops(cols))
    term%tabstops = .false.
    do i = 1, cols, 8
      term%tabstops(i) = .true.
    end do
  end subroutine terminal_init

  ! Destroy terminal
  subroutine terminal_destroy(term)
    type(terminal_t), intent(inout) :: term

    call screen_destroy(term%screen)
    call screen_destroy(term%alt_screen)
    call scrollback_destroy(term%scrollback)
    if (allocated(term%tabstops)) deallocate(term%tabstops)
  end subroutine terminal_destroy

  ! Resize terminal
  subroutine terminal_resize(term, rows, cols)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: rows, cols
    integer :: i

    term%rows = rows
    term%cols = cols
    term%scroll_bottom = rows

    call screen_resize(term%screen, rows, cols)
    call screen_resize(term%alt_screen, rows, cols)

    ! Resize tab stops
    if (allocated(term%tabstops)) deallocate(term%tabstops)
    allocate(term%tabstops(cols))
    term%tabstops = .false.
    do i = 1, cols, 8
      term%tabstops(i) = .true.
    end do

    ! Clamp cursor to new bounds
    term%cursor%row = min(term%cursor%row, rows)
    term%cursor%col = min(term%cursor%col, cols)
  end subroutine terminal_resize

  ! Get pointer to active screen
  function terminal_active_screen(term) result(scr)
    type(terminal_t), intent(inout), target :: term
    type(screen_t), pointer :: scr

    if (term%using_alt) then
      scr => term%alt_screen
    else
      scr => term%screen
    end if
  end function terminal_active_screen

  ! Put a character at cursor position
  subroutine terminal_put_char(term, codepoint)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: codepoint
    type(cell_t) :: cell, cont_cell
    type(screen_t), pointer :: scr
    integer :: width

    scr => terminal_active_screen(term)

    ! Handle control characters
    select case (codepoint)
      case (10)  ! LF - Line Feed
        call terminal_newline(term)
        return
      case (13)  ! CR - Carriage Return
        call terminal_carriage_return(term)
        return
      case (9)   ! HT - Horizontal Tab
        call terminal_tab(term)
        return
      case (8)   ! BS - Backspace
        call terminal_backspace(term)
        return
      case (7)   ! BEL - Bell (ignore for now)
        return
      case (0:6, 14:26, 28:31)  ! Other control chars - ignore (27=ESC handled in Phase 5)
        return
    end select

    ! Determine character display width
    width = codepoint_width(codepoint)

    ! Skip zero-width characters (combining marks, etc.)
    if (width == 0) return

    ! Handle deferred wrap: if pending, execute wrap now before writing new char
    if (term%pending_wrap) then
      call terminal_newline(term)
      term%cursor%col = 1
      term%pending_wrap = .false.
    end if

    ! Check if wide character fits before line end
    if (width == 2 .and. term%cursor%col + 1 > term%cols) then
      if (term%mode_autowrap) then
        call terminal_newline(term)
        term%cursor%col = 1
      else
        ! Can't fit - don't draw
        return
      end if
    end if

    ! Printable character - create cell with current style
    cell%codepoint = codepoint
    cell%fg = term%cursor%fg
    cell%bg = term%cursor%bg
    cell%attrs = term%cursor%attrs
    cell%width = width
    cell%is_continuation = .false.

    ! Place in buffer
    call screen_set_cell(scr, term%cursor%row, term%cursor%col, cell)

    ! For wide characters, write continuation cell
    if (width == 2 .and. term%cursor%col < term%cols) then
      cont_cell%codepoint = 0
      cont_cell%fg = term%cursor%fg
      cont_cell%bg = term%cursor%bg
      cont_cell%attrs = term%cursor%attrs
      cont_cell%width = 0
      cont_cell%is_continuation = .true.
      call screen_set_cell(scr, term%cursor%row, term%cursor%col + 1, cont_cell)
    end if

    ! Advance cursor by character width
    term%cursor%col = term%cursor%col + width

    ! Handle wrap at end of line - use deferred wrap
    if (term%cursor%col > term%cols) then
      if (term%mode_autowrap) then
        ! Don't wrap yet - set pending flag, wrap on next char
        term%cursor%col = term%cols  ! Keep cursor at last column
        term%pending_wrap = .true.
      else
        term%cursor%col = term%cols  ! Stay at edge
      end if
    end if
  end subroutine terminal_put_char

  ! Move to next line
  subroutine terminal_newline(term)
    type(terminal_t), intent(inout) :: term

    if (term%cursor%row >= term%scroll_bottom) then
      ! At bottom of scroll region - scroll up
      call terminal_scroll_up(term, 1)
    else
      ! Move down
      term%cursor%row = term%cursor%row + 1
    end if
  end subroutine terminal_newline

  ! Move to start of line
  subroutine terminal_carriage_return(term)
    type(terminal_t), intent(inout) :: term

    term%cursor%col = 1
    term%pending_wrap = .false.  ! Cancel deferred wrap
  end subroutine terminal_carriage_return

  ! Move to next tab stop
  subroutine terminal_tab(term)
    type(terminal_t), intent(inout) :: term
    integer :: col

    term%pending_wrap = .false.  ! Cancel deferred wrap

    do col = term%cursor%col + 1, term%cols
      if (term%tabstops(col)) then
        term%cursor%col = col
        return
      end if
    end do

    ! No more tab stops - go to end
    term%cursor%col = term%cols
  end subroutine terminal_tab

  ! Move cursor back one position
  subroutine terminal_backspace(term)
    type(terminal_t), intent(inout) :: term

    term%pending_wrap = .false.  ! Cancel deferred wrap
    if (term%cursor%col > 1) then
      term%cursor%col = term%cursor%col - 1
    end if
  end subroutine terminal_backspace

  ! Scroll up n lines within scroll region
  subroutine terminal_scroll_up(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: row, col, src_row, i

    scr => terminal_active_screen(term)

    ! Save lines to scrollback before they're lost (primary screen only)
    ! Only save if scrolling from the very top of the screen
    if (.not. term%using_alt .and. term%scroll_top == 1) then
      do i = 1, min(n, term%scroll_bottom)
        call scrollback_push_line(term%scrollback, scr%cells(i, :), term%cols)
      end do
    end if

    ! Move lines up
    do row = term%scroll_top, term%scroll_bottom - n
      src_row = row + n
      do col = 1, term%cols
        scr%cells(row, col) = scr%cells(src_row, col)
      end do
      call screen_mark_dirty(scr, row)
    end do

    ! Clear new lines at bottom
    do row = term%scroll_bottom - n + 1, term%scroll_bottom
      call screen_clear_line(scr, row)
    end do
  end subroutine terminal_scroll_up

  ! Scroll down n lines within scroll region
  subroutine terminal_scroll_down(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: row, col, src_row

    scr => terminal_active_screen(term)

    ! Move lines down (iterate in reverse)
    do row = term%scroll_bottom, term%scroll_top + n, -1
      src_row = row - n
      do col = 1, term%cols
        scr%cells(row, col) = scr%cells(src_row, col)
      end do
      call screen_mark_dirty(scr, row)
    end do

    ! Clear new lines at top
    do row = term%scroll_top, term%scroll_top + n - 1
      call screen_clear_line(scr, row)
    end do
  end subroutine terminal_scroll_down

  ! Erase in display (ED)
  ! mode 0: cursor to end, mode 1: start to cursor, mode 2: entire screen
  subroutine terminal_erase_display(term, mode)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: mode
    type(screen_t), pointer :: scr
    integer :: row

    scr => terminal_active_screen(term)

    select case (mode)
      case (0)  ! Cursor to end of screen
        ! Clear from cursor to end of current line
        call screen_clear_region(scr, term%cursor%row, term%cursor%col, &
                                 term%cursor%row, term%cols)
        ! Clear remaining lines
        do row = term%cursor%row + 1, term%rows
          call screen_clear_line(scr, row)
        end do

      case (1)  ! Start to cursor
        ! Clear lines before cursor
        do row = 1, term%cursor%row - 1
          call screen_clear_line(scr, row)
        end do
        ! Clear from start of line to cursor
        call screen_clear_region(scr, term%cursor%row, 1, &
                                 term%cursor%row, term%cursor%col)

      case (2, 3)  ! Entire screen (3 also clears scrollback, but we don't have that yet)
        call screen_clear(scr)
    end select
  end subroutine terminal_erase_display

  ! Erase in line (EL)
  ! mode 0: cursor to end, mode 1: start to cursor, mode 2: entire line
  subroutine terminal_erase_line(term, mode)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: mode
    type(screen_t), pointer :: scr

    scr => terminal_active_screen(term)

    select case (mode)
      case (0)  ! Cursor to end of line
        call screen_clear_region(scr, term%cursor%row, term%cursor%col, &
                                 term%cursor%row, term%cols)

      case (1)  ! Start to cursor
        call screen_clear_region(scr, term%cursor%row, 1, &
                                 term%cursor%row, term%cursor%col)

      case (2)  ! Entire line
        call screen_clear_line(scr, term%cursor%row)
    end select
  end subroutine terminal_erase_line

  ! Move cursor to absolute position
  subroutine terminal_cursor_move(term, row, col)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: row, col

    term%pending_wrap = .false.  ! Cancel deferred wrap
    term%cursor%row = max(1, min(row, term%rows))
    term%cursor%col = max(1, min(col, term%cols))
  end subroutine terminal_cursor_move

  ! Move cursor up n rows
  subroutine terminal_cursor_up(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n

    term%pending_wrap = .false.  ! Cancel deferred wrap
    term%cursor%row = max(1, term%cursor%row - n)
  end subroutine terminal_cursor_up

  ! Move cursor down n rows
  subroutine terminal_cursor_down(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n

    term%pending_wrap = .false.  ! Cancel deferred wrap
    term%cursor%row = min(term%rows, term%cursor%row + n)
  end subroutine terminal_cursor_down

  ! Move cursor forward n columns
  subroutine terminal_cursor_forward(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n

    term%pending_wrap = .false.  ! Cancel deferred wrap
    term%cursor%col = min(term%cols, term%cursor%col + n)
  end subroutine terminal_cursor_forward

  ! Move cursor backward n columns
  subroutine terminal_cursor_backward(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n

    term%pending_wrap = .false.  ! Cancel deferred wrap
    term%cursor%col = max(1, term%cursor%col - n)
  end subroutine terminal_cursor_backward

  ! Switch between primary and alternate screen
  subroutine terminal_switch_screen(term, use_alt)
    type(terminal_t), intent(inout) :: term
    logical, intent(in) :: use_alt

    if (use_alt .and. .not. term%using_alt) then
      ! Switching to alternate - save cursor
      term%saved_cursor = term%cursor
      term%using_alt = .true.
      call screen_clear(term%alt_screen)
      term%cursor%row = 1
      term%cursor%col = 1
    else if (.not. use_alt .and. term%using_alt) then
      ! Switching back to primary - restore cursor
      term%using_alt = .false.
      term%cursor = term%saved_cursor
    end if

    ! Mark active screen as fully dirty
    if (term%using_alt) then
      call screen_mark_all_dirty(term%alt_screen)
    else
      call screen_mark_all_dirty(term%screen)
    end if
  end subroutine terminal_switch_screen

  ! Save cursor position and attributes (DECSC)
  subroutine terminal_save_cursor(term)
    type(terminal_t), intent(inout) :: term

    term%saved_cursor = term%cursor
  end subroutine terminal_save_cursor

  ! Restore cursor position and attributes (DECRC)
  subroutine terminal_restore_cursor(term)
    type(terminal_t), intent(inout) :: term

    term%cursor = term%saved_cursor
    ! Clamp to current screen bounds
    term%cursor%row = max(1, min(term%cursor%row, term%rows))
    term%cursor%col = max(1, min(term%cursor%col, term%cols))
  end subroutine terminal_restore_cursor

  ! Set scroll region (DECSTBM)
  subroutine terminal_set_scroll_region(term, top, bottom)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: top, bottom

    if (top < bottom .and. top >= 1 .and. bottom <= term%rows) then
      term%scroll_top = top
      term%scroll_bottom = bottom
      ! Move cursor to home position
      if (term%mode_origin) then
        term%cursor%row = top
      else
        term%cursor%row = 1
      end if
      term%cursor%col = 1
    end if
  end subroutine terminal_set_scroll_region

  ! Insert n blank lines at cursor (IL)
  subroutine terminal_insert_lines(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: row, col, src_row, actual_n

    ! Only works within scroll region
    if (term%cursor%row < term%scroll_top .or. term%cursor%row > term%scroll_bottom) return

    scr => terminal_active_screen(term)
    actual_n = min(n, term%scroll_bottom - term%cursor%row + 1)

    ! Move lines down (iterate in reverse)
    do row = term%scroll_bottom, term%cursor%row + actual_n, -1
      src_row = row - actual_n
      do col = 1, term%cols
        scr%cells(row, col) = scr%cells(src_row, col)
      end do
      call screen_mark_dirty(scr, row)
    end do

    ! Clear new lines at cursor position
    do row = term%cursor%row, term%cursor%row + actual_n - 1
      call screen_clear_line(scr, row)
    end do
  end subroutine terminal_insert_lines

  ! Delete n lines at cursor (DL)
  subroutine terminal_delete_lines(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: row, col, src_row, actual_n

    ! Only works within scroll region
    if (term%cursor%row < term%scroll_top .or. term%cursor%row > term%scroll_bottom) return

    scr => terminal_active_screen(term)
    actual_n = min(n, term%scroll_bottom - term%cursor%row + 1)

    ! Move lines up
    do row = term%cursor%row, term%scroll_bottom - actual_n
      src_row = row + actual_n
      do col = 1, term%cols
        scr%cells(row, col) = scr%cells(src_row, col)
      end do
      call screen_mark_dirty(scr, row)
    end do

    ! Clear lines at bottom of scroll region
    do row = term%scroll_bottom - actual_n + 1, term%scroll_bottom
      call screen_clear_line(scr, row)
    end do
  end subroutine terminal_delete_lines

  ! Insert n blank characters at cursor (ICH)
  subroutine terminal_insert_chars(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: col, src_col, actual_n

    scr => terminal_active_screen(term)
    actual_n = min(n, term%cols - term%cursor%col + 1)

    ! Shift characters right
    do col = term%cols, term%cursor%col + actual_n, -1
      src_col = col - actual_n
      scr%cells(term%cursor%row, col) = scr%cells(term%cursor%row, src_col)
    end do

    ! Clear inserted positions
    do col = term%cursor%col, term%cursor%col + actual_n - 1
      scr%cells(term%cursor%row, col) = cell_t(32, default_fg, default_bg, 0)
    end do

    call screen_mark_dirty(scr, term%cursor%row)
  end subroutine terminal_insert_chars

  ! Delete n characters at cursor (DCH)
  subroutine terminal_delete_chars(term, n)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: n
    type(screen_t), pointer :: scr
    integer :: col, src_col, actual_n

    scr => terminal_active_screen(term)
    actual_n = min(n, term%cols - term%cursor%col + 1)

    ! Shift characters left
    do col = term%cursor%col, term%cols - actual_n
      src_col = col + actual_n
      scr%cells(term%cursor%row, col) = scr%cells(term%cursor%row, src_col)
    end do

    ! Clear vacated positions at end
    do col = term%cols - actual_n + 1, term%cols
      scr%cells(term%cursor%row, col) = cell_t(32, default_fg, default_bg, 0)
    end do

    call screen_mark_dirty(scr, term%cursor%row)
  end subroutine terminal_delete_chars

  ! Index - move cursor down, scroll if at bottom (IND)
  subroutine terminal_index(term)
    type(terminal_t), intent(inout) :: term

    if (term%cursor%row >= term%scroll_bottom) then
      call terminal_scroll_up(term, 1)
    else
      term%cursor%row = term%cursor%row + 1
    end if
  end subroutine terminal_index

  ! Reverse index - move cursor up, scroll if at top (RI)
  subroutine terminal_reverse_index(term)
    type(terminal_t), intent(inout) :: term

    if (term%cursor%row <= term%scroll_top) then
      call terminal_scroll_down(term, 1)
    else
      term%cursor%row = term%cursor%row - 1
    end if
  end subroutine terminal_reverse_index

  ! Reset terminal to initial state (RIS)
  subroutine terminal_reset(term)
    type(terminal_t), intent(inout) :: term
    integer :: i

    ! Reset scroll region
    term%scroll_top = 1
    term%scroll_bottom = term%rows

    ! Reset modes
    term%mode_autowrap = .true.
    term%mode_origin = .false.
    term%mode_insert = .false.
    term%pending_wrap = .false.

    ! Reset cursor
    term%cursor%row = 1
    term%cursor%col = 1
    term%cursor%fg = default_fg
    term%cursor%bg = default_bg
    term%cursor%attrs = 0
    term%cursor%visible = .true.

    ! Clear screen
    call screen_clear(term%screen)
    call screen_clear(term%alt_screen)

    ! Switch to primary screen
    term%using_alt = .false.

    ! Reset tab stops
    term%tabstops = .false.
    do i = 1, term%cols, 8
      term%tabstops(i) = .true.
    end do
  end subroutine terminal_reset

  ! Scroll view into scrollback history
  ! Positive delta = scroll up (back in history)
  ! Negative delta = scroll down (toward present)
  subroutine terminal_scroll_view(term, delta)
    type(terminal_t), intent(inout) :: term
    integer, intent(in) :: delta
    integer :: max_offset

    ! Don't scroll on alternate screen
    if (term%using_alt) return

    max_offset = scrollback_count(term%scrollback)
    term%scroll_offset = term%scroll_offset + delta
    term%scroll_offset = max(0, min(term%scroll_offset, max_offset))
  end subroutine terminal_scroll_view

  ! Get current scroll offset
  function terminal_get_scroll_offset(term) result(offset)
    type(terminal_t), intent(in) :: term
    integer :: offset

    offset = term%scroll_offset
  end function terminal_get_scroll_offset

  ! Reset scroll view to live (current) output
  subroutine terminal_reset_scroll_view(term)
    type(terminal_t), intent(inout) :: term

    term%scroll_offset = 0
  end subroutine terminal_reset_scroll_view

  ! Get number of lines in scrollback
  function terminal_get_scrollback_count(term) result(n)
    type(terminal_t), intent(in) :: term
    integer :: n

    n = scrollback_count(term%scrollback)
  end function terminal_get_scrollback_count

  ! Get a line from scrollback
  ! offset: 0 = most recent line, 1 = second most recent, etc.
  subroutine terminal_get_scrollback_line(term, offset, line, cols)
    type(terminal_t), intent(in) :: term
    integer, intent(in) :: offset
    type(cell_t), intent(out) :: line(:)
    integer, intent(in) :: cols

    call scrollback_get_line(term%scrollback, offset, line, cols)
  end subroutine terminal_get_scrollback_line

  ! Queue a response to be sent back to the PTY
  subroutine terminal_queue_response(term, response)
    type(terminal_t), intent(inout) :: term
    character(len=*), intent(in) :: response
    integer :: len_resp

    len_resp = len_trim(response)
    if (len_resp > 0 .and. len_resp <= RESPONSE_BUFFER_SIZE) then
      term%response = response
      term%response_len = len_resp
    end if
  end subroutine terminal_queue_response

  ! Check if there's a pending response
  function terminal_has_response(term) result(has)
    type(terminal_t), intent(in) :: term
    logical :: has

    has = (term%response_len > 0)
  end function terminal_has_response

  ! Get and clear the pending response
  subroutine terminal_get_response(term, response, length)
    type(terminal_t), intent(inout) :: term
    character(len=*), intent(out) :: response
    integer, intent(out) :: length

    response = term%response
    length = term%response_len
    term%response = ''
    term%response_len = 0
  end subroutine terminal_get_response

  ! Set window title (from OSC 0/1/2)
  subroutine terminal_set_title(term, title)
    type(terminal_t), intent(inout) :: term
    character(len=*), intent(in) :: title

    term%title = title
    term%title_changed = .true.
  end subroutine terminal_set_title

  ! Get window title
  function terminal_get_title(term) result(title)
    type(terminal_t), intent(in) :: term
    character(len=256) :: title

    title = term%title
  end function terminal_get_title

  ! Check if title has changed (and clear the flag)
  function terminal_has_title_changed(term) result(changed)
    type(terminal_t), intent(inout) :: term
    logical :: changed

    changed = term%title_changed
    term%title_changed = .false.
  end function terminal_has_title_changed

end module terminal_mod
