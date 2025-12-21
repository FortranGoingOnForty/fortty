program fortty
  use window_mod
  use selection_mod, only: selection_contains
  use gl_bindings
  use renderer_mod
  use font_mod, only: font_find_monospace, font_find_for_codepoint
  use pty_mod
  use terminal_mod
  use parser_mod
  use screen_mod
  use cell_mod, only: cell_t, set_palette_color, set_default_colors
  use config_mod
  use cursor_mod, only: CURSOR_BLOCK, CURSOR_UNDERLINE, CURSOR_BAR
  use glfw_bindings, only: glfwGetTime
  implicit none

  type(window_t) :: win
  type(renderer_t), target :: ren
  type(pty_t) :: pty
  type(terminal_t) :: term
  type(parser_t) :: parser
  type(screen_t), pointer :: scr
  type(cell_t) :: cell
  type(config_t) :: cfg
  integer :: win_width, win_height
  integer :: prev_width, prev_height
  integer :: term_rows, term_cols
  integer :: new_rows, new_cols
  character(len=256) :: font_path, fallback_path
  character(len=4096) :: pty_buffer
  character(len=256) :: response_buf
  integer :: nbytes, i, row, col, scroll_offset, sb_offset, screen_row
  integer :: response_len
  real :: x, y, r, g, b, bg_r, bg_g, bg_b
  type(cell_t), allocatable :: sb_line(:)
  integer :: cell_width, cell_height, ascender  ! From font metrics
  real(8) :: current_time, last_time, blink_timer
  logical :: cursor_blink_visible
  type(selection_t) :: sel

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
    print *, "Fallback font loaded: ", trim(fallback_path)
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
  term_cols = win_width / cell_width
  term_rows = win_height / cell_height
  prev_width = win_width
  prev_height = win_height

  ! Initialize terminal state and parser
  call terminal_init(term, term_rows, term_cols)
  call parser_init(parser)

  ! Apply cursor settings from config
  term%cursor%style = cfg%cursor_style
  term%cursor%blink = cfg%cursor_blink

  ! Open PTY with shell
  pty = pty_open("", term_rows, term_cols)  ! Empty string = use $SHELL

  if (.not. pty%active) then
    print *, "Error: Could not open PTY"
    call terminal_destroy(term)
    call renderer_destroy(ren)
    call window_destroy(win)
    stop 1
  end if

  ! Connect PTY and terminal to window for keyboard input and scrollback
  call window_set_pty(pty)
  call window_set_terminal(term)

  ! Initialize blink timer
  last_time = glfwGetTime()
  blink_timer = 0.0d0
  cursor_blink_visible = .true.

  ! Main event loop
  do while (.not. window_should_close(win) .and. pty_is_alive(pty))
    ! Update blink timer
    current_time = glfwGetTime()
    blink_timer = blink_timer + (current_time - last_time)
    last_time = current_time
    if (blink_timer > 0.5d0) then
      cursor_blink_visible = .not. cursor_blink_visible
      blink_timer = 0.0d0
    end if

    ! Check for window resize
    call window_get_size(win, win_width, win_height)
    if (win_width /= prev_width .or. win_height /= prev_height) then
      prev_width = win_width
      prev_height = win_height

      ! Update projection matrix
      call renderer_set_projection(ren, win_width, win_height)

      ! Calculate new terminal size and notify PTY and terminal
      new_cols = win_width / cell_width
      new_rows = win_height / cell_height
      if (new_cols /= term_cols .or. new_rows /= term_rows) then
        term_cols = new_cols
        term_rows = new_rows
        call pty_resize(pty, term_rows, term_cols)
        call terminal_resize(term, term_rows, term_cols)
      end if
    end if

    ! Read from PTY (non-blocking)
    nbytes = pty_read(pty, pty_buffer, 4096)
    if (nbytes > 0) then
      ! Process each byte through escape sequence parser
      do i = 1, nbytes
        call parser_process_byte(parser, term, ichar(pty_buffer(i:i)))
      end do
    end if

    ! Check for terminal responses (DA1, DSR, etc.) and send to PTY
    if (terminal_has_response(term)) then
      call terminal_get_response(term, response_buf, response_len)
      if (response_len > 0) then
        call pty_write(pty, response_buf, response_len)
      end if
    end if

    ! Check for window title changes (from OSC 0/1/2)
    if (terminal_has_title_changed(term)) then
      call window_set_title(win, terminal_get_title(term))
    end if

    ! Clear screen with background color and opacity from config
    bg_r = real(cfg%bg_color%r) / 255.0
    bg_g = real(cfg%bg_color%g) / 255.0
    bg_b = real(cfg%bg_color%b) / 255.0
    call glClearColor(bg_r, bg_g, bg_b, cfg%window_opacity)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Render terminal buffer
    call renderer_begin(ren)

    scr => terminal_active_screen(term)
    scroll_offset = terminal_get_scroll_offset(term)

    ! Get current selection for highlighting
    sel = window_get_selection()

    ! Allocate/resize scrollback line buffer if needed
    if (.not. allocated(sb_line)) then
      allocate(sb_line(term_cols))
    else if (size(sb_line) /= term_cols) then
      deallocate(sb_line)
      allocate(sb_line(term_cols))
    end if

    do row = 1, scr%rows
      ! Cell top-left y coordinate (for rectangles like selection/cursor)
      ! Row 1 starts at y=0, row 2 at y=cell_height, etc.
      y = real(row - 1) * cell_height

      ! Determine if this row shows scrollback or screen content
      sb_offset = scroll_offset - row + 1

      if (sb_offset > 0 .and. sb_offset <= terminal_get_scrollback_count(term)) then
        ! This row shows scrollback content
        call terminal_get_scrollback_line(term, sb_offset - 1, sb_line, term_cols)
        do col = 1, min(term_cols, scr%cols)
          cell = sb_line(col)

          ! Skip continuation cells (2nd half of wide chars)
          if (cell%is_continuation) cycle

          x = real(col - 1) * cell_width

          ! Draw selection background if selected (for all cells including spaces)
          if (selection_contains(sel, row, col)) then
            call renderer_draw_rect(ren, x, y, real(cell_width), real(cell_height), &
                                    0.3, 0.3, 0.6, 1.0)
          end if

          ! Only render cells with actual text content
          if (cell%codepoint /= 32 .and. cell%codepoint /= 0) then
            r = real(cell%fg%r) / 255.0
            g = real(cell%fg%g) / 255.0
            b = real(cell%fg%b) / 255.0
            ! Baseline is at y + ascender (not cell bottom - descenders need room below)
            call renderer_draw_char(ren, x, y + real(ascender), cell%codepoint, r, g, b, 1.0)
          end if
        end do
      else
        ! This row shows screen content
        screen_row = row - scroll_offset
        if (screen_row >= 1 .and. screen_row <= scr%rows) then
          do col = 1, scr%cols
            cell = screen_get_cell(scr, screen_row, col)

            ! Skip continuation cells (2nd half of wide chars)
            if (cell%is_continuation) cycle

            x = real(col - 1) * cell_width

            ! Draw selection background if selected (for all cells including spaces)
            if (selection_contains(sel, row, col)) then
              call renderer_draw_rect(ren, x, y, real(cell_width), real(cell_height), &
                                      0.3, 0.3, 0.6, 1.0)
            end if

            ! Only render cells with actual text content
            if (cell%codepoint /= 32 .and. cell%codepoint /= 0) then
              r = real(cell%fg%r) / 255.0
              g = real(cell%fg%g) / 255.0
              b = real(cell%fg%b) / 255.0
              ! Baseline is at y + ascender (consistent with scrollback rendering)
              call renderer_draw_char(ren, x, y + real(ascender), cell%codepoint, r, g, b, 1.0)
            end if
          end do
        end if
      end if
    end do

    ! Draw cursor if visible (only when not scrolled back)
    if (term%cursor%visible .and. scroll_offset == 0) then
      ! Check blink state - only hide cursor if blink is enabled and in off phase
      if (.not. term%cursor%blink .or. cursor_blink_visible) then
        x = real(term%cursor%col - 1) * cell_width
        y = real(term%cursor%row - 1) * cell_height

        select case (term%cursor%style)
          case (CURSOR_BLOCK)
            ! Filled block cursor - cover the full cell
            call renderer_draw_rect(ren, x, y, real(cell_width), real(cell_height), &
                                    0.7, 0.7, 0.7, 0.8)
          case (CURSOR_UNDERLINE)
            ! Underline at the baseline position (y + ascender)
            call renderer_draw_rect(ren, x, y + real(ascender), &
                                    real(cell_width), 2.0, 0.7, 0.7, 0.7, 1.0)
          case (CURSOR_BAR)
            ! Vertical bar at left of cell - full cell height
            call renderer_draw_rect(ren, x, y, 2.0, real(cell_height), 0.7, 0.7, 0.7, 1.0)
          case default
            ! Fallback to block
            call renderer_draw_rect(ren, x, y, real(cell_width), real(cell_height), &
                                    0.7, 0.7, 0.7, 0.8)
        end select
      end if
    end if

    call renderer_flush(ren)

    ! Swap buffers and poll events
    call window_swap_buffers(win)
    call window_poll_events()
  end do

  ! Cleanup
  if (allocated(sb_line)) deallocate(sb_line)
  call pty_close(pty)
  call terminal_destroy(term)
  call renderer_destroy(ren)
  call window_destroy(win)

end program fortty
