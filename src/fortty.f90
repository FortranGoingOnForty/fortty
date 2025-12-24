program fortty
  use window_mod
  use gl_bindings
  use renderer_mod
  use font_mod, only: font_find_monospace, font_find_for_codepoint
  use pty_mod
  use terminal_mod
  use parser_mod
  use screen_mod
  use cell_mod, only: set_palette_color, set_default_colors
  use config_mod
  use glfw_bindings, only: glfwGetTime
  use tab_manager_mod
  use tab_bar_mod
  use pane_mod
  use layout_mod, only: DIR_LEFT, DIR_RIGHT, DIR_UP, DIR_DOWN
  use render_state_mod, only: render_state_init, render_state_update_blink, do_render
  implicit none

  type(window_t) :: win
  type(renderer_t), target :: ren
  type(tab_manager_t), target :: tab_mgr
  type(pane_t), pointer :: active_pane
  type(terminal_t), pointer :: term
  type(pty_t), pointer :: active_pty
  type(config_t) :: cfg
  integer :: win_width, win_height
  integer :: prev_width, prev_height
  integer :: term_rows, term_cols
  integer :: new_rows, new_cols
  character(len=256) :: font_path, fallback_path
  character(len=4096) :: pty_buffer
  character(len=256) :: response_buf
  integer :: nbytes, i, j, k
  integer :: response_len, tab_action, pane_action, tab_bar_height
  integer :: cell_width, cell_height, ascender  ! From font metrics
  real(8) :: current_time, last_time, blink_timer
  logical :: cursor_blink_visible, any_pty_alive
  logical :: was_focused, is_focused
  integer :: font_delta, new_font_size, base_font_size
  character(len=256) :: font_path_saved, fallback_path_saved

  ! Load configuration (uses defaults if no config file found)
  cfg = config_load('')

  ! Apply color palette from config
  call set_default_colors(cfg%fg_color, cfg%bg_color)
  do i = 0, 15
    call set_palette_color(i, cfg%palette(i))
  end do

  ! Window dimensions from config
  win_width = cfg%window_width
  win_height = cfg%window_height

  ! Create window with OpenGL context (enable transparency if opacity < 1.0)
  win = window_create(win_width, win_height, "fortty", cfg%window_opacity < 1.0)

  ! Enable background blur if configured (macOS only)
  if (cfg%window_blur) then
    call window_set_blur(win, .true.)
  end if

  ! Font path - use config if specified, otherwise fontconfig, then fallbacks
  if (len_trim(cfg%font_path) > 0) then
    font_path = cfg%font_path
    print *, "Using configured font: ", trim(font_path)
  else
    font_path = font_find_monospace()
    if (len_trim(font_path) == 0) then
      ! Fontconfig not available or failed - try common system locations
      font_path = "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
    else
      print *, "Using system monospace font: ", trim(font_path)
    end if
  end if

  ! Create renderer with font (using font size from config)
  ren = renderer_create(trim(font_path), cfg%font_size)
  if (.not. ren%initialized) then
    print *, "Warning: Could not load font, trying alternate path..."
    font_path = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    ren = renderer_create(trim(font_path), cfg%font_size)
  end if

  if (.not. ren%initialized) then
    print *, "Error: Could not initialize renderer"
    print *, "Please ensure a monospace font is installed"
    call window_destroy(win)
    stop 1
  end if

  ! Fix dangling pointer: atlas%font pointed to local var in renderer_create
  ! After assignment, we need to update it to point to ren%font
  ren%atlas%font => ren%font

  ! Save font path and base size for runtime font size changes
  font_path_saved = font_path
  base_font_size = cfg%font_size
  fallback_path_saved = ''  ! Will be set when fallback is loaded

  ! Load fallback font for missing glyphs (icons, symbols, etc.)
  ! Use config fallback if specified, otherwise auto-detect via fontconfig
  if (len_trim(cfg%font_fallback) > 0) then
    call renderer_load_fallback_font(ren, trim(cfg%font_fallback))
    if (ren%font%has_fallback) then
      print *, "Using configured fallback font: ", trim(cfg%font_fallback)
    end if
  end if

  ! If no configured fallback or it failed, try fontconfig auto-detection
  ! First try to find a font with common Unicode symbols (chevron, arrows, etc.)
  if (.not. ren%font%has_fallback) then
    fallback_path = font_find_for_codepoint(int(z'276F'))  ! Heavy right-pointing angle (❯)
    if (len_trim(fallback_path) > 0) then
      call renderer_load_fallback_font(ren, trim(fallback_path))
    end if
  end if
  ! Then try Nerd Font-specific devicons
  if (.not. ren%font%has_fallback) then
    fallback_path = font_find_for_codepoint(int(z'E5FF'))  ! Nerd Font devicon
    if (len_trim(fallback_path) > 0) then
      call renderer_load_fallback_font(ren, trim(fallback_path))
    end if
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = font_find_for_codepoint(int(z'E0A0'))  ! Powerline branch symbol
    if (len_trim(fallback_path) > 0) then
      call renderer_load_fallback_font(ren, trim(fallback_path))
    end if
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = font_find_for_codepoint(int(z'F07B'))  ! folder icon (Font Awesome)
    if (len_trim(fallback_path) > 0) then
      call renderer_load_fallback_font(ren, trim(fallback_path))
    end if
  end if

  ! If fontconfig didn't work, try hardcoded paths (macOS)
  if (.not. ren%font%has_fallback) then
    fallback_path = "/System/Library/Fonts/Apple Symbols.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = "/Library/Fonts/MesloLGLDZNerdFontMono-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = "/Library/Fonts/MesloLGMNerdFontMono-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  ! Linux paths
  if (.not. ren%font%has_fallback) then
    fallback_path = "/usr/share/fonts/TTF/MesloLGLDZNerdFontMono-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = "/usr/share/fonts/TTF/MesloLGMNerdFontMono-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  ! Fall back to Noto Sans Symbols if no Nerd Font
  if (.not. ren%font%has_fallback) then
    fallback_path = "/usr/share/fonts/noto/NotoSansSymbols2-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if
  if (.not. ren%font%has_fallback) then
    fallback_path = "/usr/share/fonts/TTF/NotoSansSymbols2-Regular.ttf"
    call renderer_load_fallback_font(ren, trim(fallback_path))
  end if

  if (ren%font%has_fallback) then
    ! Save the successful fallback path for font size changes
    if (len_trim(cfg%font_fallback) > 0) then
      fallback_path_saved = cfg%font_fallback
    else
      fallback_path_saved = fallback_path
    end if
    print *, "Fallback font loaded: ", trim(fallback_path_saved)
  else
    print *, "Warning: No fallback font loaded - some icons may not display"
  end if

  ! Set up projection matrix
  call renderer_set_projection(ren, win_width, win_height)

  ! Get cell dimensions from font metrics
  cell_width = ren%font%cell_width
  cell_height = ren%font%cell_height
  ascender = ren%font%ascender
  if (cell_width < 1) cell_width = 10  ! Fallback
  if (cell_height < 1) cell_height = 18  ! Fallback
  if (ascender < 1) ascender = cell_height - 4  ! Fallback estimate
  print *, "Font cell size:", cell_width, "x", cell_height, " ascender:", ascender

  ! Share cell dimensions with window module for mouse selection
  call window_set_cell_size(cell_width, cell_height)

  ! Calculate terminal dimensions based on font metrics
  ! Tab bar is hidden with only 1 tab, so use full height initially
  term_cols = win_width / cell_width
  term_rows = win_height / cell_height
  prev_width = win_width
  prev_height = win_height

  ! Initialize tab manager with first tab
  call tab_manager_init(tab_mgr, term_rows, term_cols)

  if (.not. tab_manager_has_tabs(tab_mgr)) then
    print *, "Error: Could not create initial tab"
    call renderer_destroy(ren)
    call window_destroy(win)
    stop 1
  end if

  ! Apply cursor settings from config to first tab's first pane
  tab_mgr%tabs(1)%panes(1)%term%cursor%style = cfg%cursor_style
  tab_mgr%tabs(1)%panes(1)%term%cursor%blink = cfg%cursor_blink

  ! Get pointers to active tab's terminal and PTY
  term => tab_manager_get_active_term(tab_mgr)
  active_pty => tab_manager_get_active_pty(tab_mgr)

  ! Connect active PTY and terminal to window for keyboard input and scrollback
  call window_set_pty(active_pty)
  call window_set_terminal(term)

  ! Initialize render state module with pointers to program state
  call render_state_init(win, ren, tab_mgr, cfg, cell_width, cell_height, ascender, &
                         win_width, win_height)

  ! Register render callback for live resize support on macOS
  call window_set_render_callback(do_render)

  ! Initialize blink timer
  last_time = glfwGetTime()
  blink_timer = 0.0d0
  cursor_blink_visible = .true.

  ! Initialize focus tracking (assume focused initially to ensure first render)
  was_focused = .true.

  ! Main event loop - exit when window closes or all tabs closed
  any_pty_alive = tab_manager_has_tabs(tab_mgr)
  do while (.not. window_should_close(win) .and. any_pty_alive)
    ! Update blink timer
    current_time = glfwGetTime()
    blink_timer = blink_timer + (current_time - last_time)
    last_time = current_time
    if (blink_timer > 0.5d0) then
      cursor_blink_visible = .not. cursor_blink_visible
      blink_timer = 0.0d0
    end if
    call render_state_update_blink(cursor_blink_visible)

    ! Poll events first - this ensures resize callbacks fire BEFORE we render
    ! so viewport and projection updates happen in the same frame
    call window_poll_events()

    ! Handle tab actions (Cmd/Ctrl+T, W, [, ], 1-9)
    tab_action = window_get_tab_action()
    if (tab_action /= 0) then
      call window_clear_tab_action()
      call handle_tab_action(tab_action)
    end if

    ! Handle pane actions (Cmd/Ctrl+\, arrows, hjkl)
    pane_action = window_get_pane_action()
    if (pane_action /= 0) then
      call window_clear_pane_action()
      call handle_pane_action(pane_action)
    end if

    ! Check for font size change request (Ctrl/Cmd +/-)
    font_delta = window_get_font_delta()
    if (font_delta /= 0) then
      call window_clear_font_delta()

      if (font_delta == -999) then
        ! Reset to default
        new_font_size = base_font_size
      else
        new_font_size = ren%font%size_px + font_delta
      end if

      ! Clamp to reasonable range (8px to 72px)
      new_font_size = max(8, min(72, new_font_size))

      if (new_font_size /= ren%font%size_px) then
        ! Reload font with new size
        call renderer_change_font_size(ren, trim(font_path_saved), new_font_size)

        ! Fix atlas font pointer after reload
        ren%atlas%font => ren%font

        ! Reload fallback font (using saved path from startup)
        if (len_trim(fallback_path_saved) > 0) then
          call renderer_load_fallback_font(ren, trim(fallback_path_saved))
        end if

        ! Update cell dimensions from new font
        cell_width = ren%font%cell_width
        cell_height = ren%font%cell_height
        ascender = ren%font%ascender
        if (cell_width < 1) cell_width = 10
        if (cell_height < 1) cell_height = 18
        if (ascender < 1) ascender = cell_height - 4

        ! Update window module's cell size for mouse coords
        call window_set_cell_size(cell_width, cell_height)

        ! Recalculate terminal dimensions (account for tab bar if visible)
        ! Tab bar is hidden when only 1 tab
        if (tab_mgr%count > 1) then
          tab_bar_height = tab_mgr%bar_height
        else
          tab_bar_height = 0
        end if
        new_cols = win_width / cell_width
        new_rows = (win_height - tab_bar_height) / cell_height
        if (new_cols /= term_cols .or. new_rows /= term_rows) then
          term_cols = new_cols
          term_rows = new_rows
          tab_mgr%term_rows = term_rows
          tab_mgr%term_cols = term_cols
          ! Resize all panes in all tabs
          do i = 1, tab_mgr%count
            do k = 1, tab_mgr%tabs(i)%pane_count
              call pty_resize(tab_mgr%tabs(i)%panes(k)%pty, term_rows, term_cols)
              call terminal_resize(tab_mgr%tabs(i)%panes(k)%term, term_rows, term_cols)
            end do
          end do
          ! Recalculate layout for active tab
          call tab_manager_recalculate_layout(tab_mgr, 0, tab_bar_height, &
                                              win_width, win_height - tab_bar_height, &
                                              cell_width, cell_height)
        end if

        call window_set_font_size(new_font_size)
        print *, "Font size:", new_font_size, "px (", term_cols, "x", term_rows, ")"
      end if
    end if

    ! Check for window resize
    call window_get_size(win, win_width, win_height)
    if (win_width /= prev_width .or. win_height /= prev_height) then
      prev_width = win_width
      prev_height = win_height

      ! Update projection matrix
      call renderer_set_projection(ren, win_width, win_height)

      ! Calculate new terminal size and notify PTY and terminal (account for tab bar if visible)
      if (tab_mgr%count > 1) then
        tab_bar_height = tab_mgr%bar_height
      else
        tab_bar_height = 0
      end if
      new_cols = win_width / cell_width
      new_rows = (win_height - tab_bar_height) / cell_height
      if (new_cols /= term_cols .or. new_rows /= term_rows) then
        term_cols = new_cols
        term_rows = new_rows
        tab_mgr%term_rows = term_rows
        tab_mgr%term_cols = term_cols
        ! Resize all panes in all tabs
        do i = 1, tab_mgr%count
          do k = 1, tab_mgr%tabs(i)%pane_count
            call pty_resize(tab_mgr%tabs(i)%panes(k)%pty, term_rows, term_cols)
            call terminal_resize(tab_mgr%tabs(i)%panes(k)%term, term_rows, term_cols)
          end do
        end do
      end if
      ! Recalculate layout for active tab
      call tab_manager_recalculate_layout(tab_mgr, 0, tab_bar_height, &
                                          win_width, win_height - tab_bar_height, &
                                          cell_width, cell_height)
    end if

    ! Read from ALL PTYs (non-blocking) - keeps inactive panes responsive
    any_pty_alive = .false.
    do i = 1, tab_mgr%count
      do k = 1, tab_mgr%tabs(i)%pane_count
        if (tab_mgr%tabs(i)%panes(k)%pty%active) then
          any_pty_alive = .true.
          nbytes = pty_read(tab_mgr%tabs(i)%panes(k)%pty, pty_buffer, 4096)
          if (nbytes > 0) then
            ! Process each byte through this pane's parser
            do j = 1, nbytes
              call parser_process_byte(tab_mgr%tabs(i)%panes(k)%parser, &
                                       tab_mgr%tabs(i)%panes(k)%term, &
                                       ichar(pty_buffer(j:j)))
            end do
          end if

          ! Check for terminal responses and send to this pane's PTY
          if (terminal_has_response(tab_mgr%tabs(i)%panes(k)%term)) then
            call terminal_get_response(tab_mgr%tabs(i)%panes(k)%term, response_buf, response_len)
            if (response_len > 0) then
              call pty_write(tab_mgr%tabs(i)%panes(k)%pty, response_buf, response_len)
            end if
          end if
        end if
      end do
    end do

    ! Update window pointers to current active tab
    term => tab_manager_get_active_term(tab_mgr)
    active_pty => tab_manager_get_active_pty(tab_mgr)
    if (associated(term) .and. associated(active_pty)) then
      call window_set_pty(active_pty)
      call window_set_terminal(term)
    end if

    ! Check for window title changes (from OSC 0/1/2) on active tab
    if (associated(term)) then
      if (terminal_has_title_changed(term)) then
        call window_set_title(win, terminal_get_title(term))
        ! Also update the tab title
        tab_mgr%tabs(tab_mgr%active_index)%title = terminal_get_title(term)
      end if
    end if

    ! Render and swap - but only if window has focus or just regained focus
    ! On Wayland, glfwSwapBuffers blocks waiting for frame callbacks when
    ! the window is on an inactive workspace, causing compositor timeout.
    ! Skip rendering when unfocused to keep event loop responsive.
    is_focused = window_is_focused(win)
    if (is_focused .or. was_focused) then
      call do_render()
    end if
    was_focused = is_focused
  end do

  ! Cleanup
  call tab_manager_destroy(tab_mgr)
  call renderer_destroy(ren)
  call window_destroy(win)

contains

  ! Handle tab action signals from keyboard
  subroutine handle_tab_action(action)
    use window_mod, only: TAB_ACTION_NEW, TAB_ACTION_CLOSE, TAB_ACTION_NEXT, TAB_ACTION_PREV
    integer, intent(in) :: action
    integer :: target_tab, old_count, effective_bar_height, new_term_rows, ii, kk
    logical :: should_close_tab

    old_count = tab_mgr%count

    select case (action)
      case (TAB_ACTION_NEW)
        ! Create new tab
        call tab_manager_add(tab_mgr)
        ! Apply cursor settings from config to new tab's first pane
        if (tab_mgr%count > 0) then
          tab_mgr%tabs(tab_mgr%count)%panes(1)%term%cursor%style = cfg%cursor_style
          tab_mgr%tabs(tab_mgr%count)%panes(1)%term%cursor%blink = cfg%cursor_blink
        end if

      case (TAB_ACTION_CLOSE)
        ! Context-aware close: close pane if multiple, otherwise close tab
        if (tab_mgr%active_index >= 1 .and. tab_mgr%active_index <= tab_mgr%count) then
          if (tab_mgr%tabs(tab_mgr%active_index)%pane_count > 1) then
            ! Multiple panes - close just the active pane
            call tab_manager_close_pane(tab_mgr, should_close_tab)
            ! Recalculate layout after pane removal
            if (tab_mgr%count > 1) then
              effective_bar_height = tab_mgr%bar_height
            else
              effective_bar_height = 0
            end if
            call tab_manager_recalculate_layout(tab_mgr, 0, effective_bar_height, &
                                                win_width, win_height - effective_bar_height, &
                                                cell_width, cell_height)
          else
            ! Single pane - close the tab
            call tab_manager_close(tab_mgr, tab_mgr%active_index)
          end if
        end if

      case (TAB_ACTION_NEXT)
        ! Switch to next tab
        call tab_manager_next(tab_mgr)

      case (TAB_ACTION_PREV)
        ! Switch to previous tab
        call tab_manager_prev(tab_mgr)

      case default
        ! Check for goto tab 1-9 (actions 10-18)
        if (action >= 10 .and. action <= 18) then
          target_tab = action - 9  ! 10 -> tab 1, 11 -> tab 2, etc.
          if (target_tab <= tab_mgr%count) then
            call tab_manager_switch(tab_mgr, target_tab)
          end if
        end if
    end select

    ! Check if tab bar visibility changed (1 <-> 2+ tabs)
    ! If so, resize all terminals to account for new available height
    if ((old_count == 1 .and. tab_mgr%count > 1) .or. &
        (old_count > 1 .and. tab_mgr%count == 1)) then
      if (tab_mgr%count > 1) then
        effective_bar_height = tab_mgr%bar_height
      else
        effective_bar_height = 0
      end if
      new_term_rows = (win_height - effective_bar_height) / cell_height
      if (new_term_rows /= term_rows) then
        term_rows = new_term_rows
        tab_mgr%term_rows = term_rows
        do ii = 1, tab_mgr%count
          do kk = 1, tab_mgr%tabs(ii)%pane_count
            call pty_resize(tab_mgr%tabs(ii)%panes(kk)%pty, term_rows, term_cols)
            call terminal_resize(tab_mgr%tabs(ii)%panes(kk)%term, term_rows, term_cols)
          end do
        end do
      end if
      ! Recalculate layout for active tab
      call tab_manager_recalculate_layout(tab_mgr, 0, effective_bar_height, &
                                          win_width, win_height - effective_bar_height, &
                                          cell_width, cell_height)
    end if

    ! Update pointers after tab change
    term => tab_manager_get_active_term(tab_mgr)
    active_pty => tab_manager_get_active_pty(tab_mgr)
  end subroutine handle_tab_action

  ! Handle pane action signals from keyboard
  subroutine handle_pane_action(action)
    use window_mod, only: PANE_ACTION_SPLIT_V, PANE_ACTION_SPLIT_H, &
                          PANE_ACTION_NAV_LEFT, PANE_ACTION_NAV_RIGHT, &
                          PANE_ACTION_NAV_UP, PANE_ACTION_NAV_DOWN
    integer, intent(in) :: action
    integer :: effective_bar_height

    ! Calculate effective tab bar height
    if (tab_mgr%count > 1) then
      effective_bar_height = tab_mgr%bar_height
    else
      effective_bar_height = 0
    end if

    select case (action)
      case (PANE_ACTION_SPLIT_V)
        ! Split vertically (side-by-side)
        call tab_manager_split_pane_v(tab_mgr)
        ! Apply cursor settings from config to new pane
        active_pane => tab_manager_get_active_pane(tab_mgr)
        if (associated(active_pane)) then
          active_pane%term%cursor%style = cfg%cursor_style
          active_pane%term%cursor%blink = cfg%cursor_blink
        end if
        ! Recalculate layout
        call tab_manager_recalculate_layout(tab_mgr, 0, effective_bar_height, &
                                            win_width, win_height - effective_bar_height, &
                                            cell_width, cell_height)

      case (PANE_ACTION_SPLIT_H)
        ! Split horizontally (stacked)
        call tab_manager_split_pane_h(tab_mgr)
        ! Apply cursor settings from config to new pane
        active_pane => tab_manager_get_active_pane(tab_mgr)
        if (associated(active_pane)) then
          active_pane%term%cursor%style = cfg%cursor_style
          active_pane%term%cursor%blink = cfg%cursor_blink
        end if
        ! Recalculate layout
        call tab_manager_recalculate_layout(tab_mgr, 0, effective_bar_height, &
                                            win_width, win_height - effective_bar_height, &
                                            cell_width, cell_height)

      case (PANE_ACTION_NAV_LEFT)
        call tab_manager_navigate_pane(tab_mgr, DIR_LEFT)

      case (PANE_ACTION_NAV_RIGHT)
        call tab_manager_navigate_pane(tab_mgr, DIR_RIGHT)

      case (PANE_ACTION_NAV_UP)
        call tab_manager_navigate_pane(tab_mgr, DIR_UP)

      case (PANE_ACTION_NAV_DOWN)
        call tab_manager_navigate_pane(tab_mgr, DIR_DOWN)
    end select

    ! Update pointers after pane change
    term => tab_manager_get_active_term(tab_mgr)
    active_pty => tab_manager_get_active_pty(tab_mgr)
  end subroutine handle_pane_action

end program fortty
