module tab_bar_mod
  use renderer_mod
  use tab_manager_mod
  implicit none
  private

  public :: tab_bar_render
  public :: tab_bar_get_close_button_bounds
  public :: CLOSE_BTN_SIZE, CLOSE_BTN_MARGIN

  ! Tab bar styling constants
  real, parameter :: TAB_PADDING = 8.0      ! Horizontal padding inside tab
  real, parameter :: TAB_MIN_WIDTH = 80.0   ! Minimum tab width
  real, parameter :: TAB_MAX_WIDTH = 200.0  ! Maximum tab width
  real, parameter :: CLOSE_BTN_SIZE = 14.0  ! Close button size (square)
  real, parameter :: CLOSE_BTN_MARGIN = 6.0 ! Margin from tab right edge

contains

  ! Get close button bounds for a specific tab (for hit testing)
  subroutine tab_bar_get_close_button_bounds(tab_index, tab_count, win_width, bar_height, &
                                              btn_x, btn_y, btn_size)
    integer, intent(in) :: tab_index, tab_count, win_width, bar_height
    real, intent(out) :: btn_x, btn_y, btn_size
    real :: tab_width, tab_start_x

    tab_width = real(win_width) / real(tab_count)
    if (tab_width > TAB_MAX_WIDTH) tab_width = TAB_MAX_WIDTH
    if (tab_width < TAB_MIN_WIDTH) tab_width = TAB_MIN_WIDTH

    tab_start_x = real(tab_index - 1) * tab_width
    btn_size = CLOSE_BTN_SIZE
    btn_x = tab_start_x + tab_width - CLOSE_BTN_MARGIN - CLOSE_BTN_SIZE
    btn_y = (real(bar_height) - CLOSE_BTN_SIZE) / 2.0
  end subroutine tab_bar_get_close_button_bounds

  ! Render the tab bar at the top of the window
  subroutine tab_bar_render(ren, mgr, win_width, bar_height, cell_width, ascender, &
                            hover_tab, hover_close_btn)
    type(renderer_t), intent(inout) :: ren
    type(tab_manager_t), intent(in) :: mgr
    integer, intent(in) :: win_width, bar_height, cell_width, ascender
    integer, intent(in) :: hover_tab       ! Which tab mouse is over (0 = none)
    logical, intent(in) :: hover_close_btn ! Is mouse over close button?
    integer :: i, title_len
    real :: x, y, tab_width
    real :: bg_r, bg_g, bg_b
    real :: active_bg_r, active_bg_g, active_bg_b
    real :: inactive_bg_r, inactive_bg_g, inactive_bg_b
    real :: text_r, text_g, text_b
    real :: divider_r, divider_g, divider_b
    real :: close_btn_x, close_btn_y
    real :: close_r, close_g, close_b
    character(len=256) :: display_title
    integer :: max_chars

    if (mgr%count <= 1) return

    ! Colors (slightly darker than terminal background for contrast)
    ! Tab bar background
    bg_r = 0.08
    bg_g = 0.08
    bg_b = 0.10

    ! Active tab (lighter)
    active_bg_r = 0.15
    active_bg_g = 0.15
    active_bg_b = 0.18

    ! Inactive tab (darker)
    inactive_bg_r = 0.10
    inactive_bg_g = 0.10
    inactive_bg_b = 0.12

    ! Text color
    text_r = 0.85
    text_g = 0.85
    text_b = 0.85

    ! Divider color
    divider_r = 0.25
    divider_g = 0.25
    divider_b = 0.28

    ! Draw tab bar background
    call renderer_draw_rect(ren, 0.0, 0.0, real(win_width), real(bar_height), &
                            bg_r, bg_g, bg_b, 1.0)

    ! Calculate tab width (evenly distributed, with min/max constraints)
    if (mgr%count > 0) then
      tab_width = real(win_width) / real(mgr%count)
      if (tab_width > TAB_MAX_WIDTH) tab_width = TAB_MAX_WIDTH
      if (tab_width < TAB_MIN_WIDTH) tab_width = TAB_MIN_WIDTH
    else
      tab_width = TAB_MIN_WIDTH
    end if

    ! Maximum characters that fit in a tab (account for close button)
    max_chars = int((tab_width - 2.0 * TAB_PADDING - CLOSE_BTN_SIZE - CLOSE_BTN_MARGIN) / real(cell_width))
    if (max_chars < 3) max_chars = 3

    x = 0.0
    y = 0.0

    ! Draw each tab
    do i = 1, mgr%count
      ! Tab background
      if (i == mgr%active_index) then
        call renderer_draw_rect(ren, x + 1.0, y, tab_width - 2.0, real(bar_height), &
                                active_bg_r, active_bg_g, active_bg_b, 1.0)
      else
        call renderer_draw_rect(ren, x + 1.0, y, tab_width - 2.0, real(bar_height), &
                                inactive_bg_r, inactive_bg_g, inactive_bg_b, 1.0)
      end if

      ! Prepare display title (truncate if needed)
      title_len = len_trim(mgr%tabs(i)%title)
      if (title_len > max_chars) then
        display_title = mgr%tabs(i)%title(1:max_chars-2) // '..'
        title_len = max_chars
      else
        display_title = mgr%tabs(i)%title
      end if

      ! Draw tab title
      ! Center text vertically: y + (bar_height - cell_height) / 2 + ascender
      ! But we use ascender as baseline offset
      call renderer_draw_string(ren, x + TAB_PADDING, &
                                real(bar_height) / 2.0 + real(ascender) / 2.0, &
                                trim(display_title), text_r, text_g, text_b, 1.0)

      ! Draw close button (X)
      close_btn_x = x + tab_width - CLOSE_BTN_MARGIN - CLOSE_BTN_SIZE
      close_btn_y = (real(bar_height) - CLOSE_BTN_SIZE) / 2.0

      ! Close button color: red on hover, gray otherwise
      if (i == hover_tab .and. hover_close_btn) then
        ! Hovered - show red background circle and white X
        call renderer_draw_rect(ren, close_btn_x, close_btn_y, &
                                CLOSE_BTN_SIZE, CLOSE_BTN_SIZE, &
                                0.8, 0.2, 0.2, 1.0)
        close_r = 1.0
        close_g = 1.0
        close_b = 1.0
      else
        ! Not hovered - subtle gray X
        close_r = 0.5
        close_g = 0.5
        close_b = 0.5
      end if

      ! Draw X character centered in the button
      call renderer_draw_string(ren, close_btn_x + 2.0, &
                                close_btn_y + real(ascender) - 2.0, &
                                'x', close_r, close_g, close_b, 1.0)

      ! Draw vertical divider after tab (except last)
      if (i < mgr%count) then
        call renderer_draw_rect(ren, x + tab_width - 1.0, 4.0, 1.0, real(bar_height - 8), &
                                divider_r, divider_g, divider_b, 1.0)
      end if

      x = x + tab_width
    end do

    ! Draw bottom border line
    call renderer_draw_rect(ren, 0.0, real(bar_height - 1), real(win_width), 1.0, &
                            divider_r, divider_g, divider_b, 1.0)

  end subroutine tab_bar_render

end module tab_bar_mod
