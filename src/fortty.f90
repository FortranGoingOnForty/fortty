program fortty
  use window_mod
  use gl_bindings
  use renderer_mod
  use font_mod, only: font_find_monospace, font_find_for_codepoint
  use pty_mod
  use terminal_mod
  use parser_mod
  use screen_mod
  use cell_mod
  implicit none

  type(window_t) :: win
  type(renderer_t), target :: ren
  type(pty_t) :: pty
  type(terminal_t) :: term
  type(parser_t) :: parser
  type(screen_t), pointer :: scr
  type(cell_t) :: cell
  integer :: win_width, win_height
  integer :: prev_width, prev_height
  integer :: term_rows, term_cols
  integer :: new_rows, new_cols
  character(len=256) :: font_path, fallback_path
  character(len=4096) :: pty_buffer
  integer :: nbytes, i, row, col, scroll_offset, sb_offset, screen_row
  real :: x, y, r, g, b
  type(cell_t), allocatable :: sb_line(:)
  integer, parameter :: CELL_WIDTH = 10   ! Approximate char width
  integer, parameter :: CELL_HEIGHT = 18  ! Approximate line height

  ! Window dimensions
  win_width = 800
  win_height = 600

  ! Create window with OpenGL context
  win = window_create(win_width, win_height, "fortty")

  ! Font path - use fontconfig for portable discovery, with fallbacks
  font_path = font_find_monospace()
  if (len_trim(font_path) == 0) then
    ! Fontconfig not available or failed - try common system locations
    font_path = "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
  else
    print *, "Using system monospace font: ", trim(font_path)
  end if

  ! Create renderer with font
  ren = renderer_create(trim(font_path), 16)
  if (.not. ren%initialized) then
    print *, "Warning: Could not load font, trying alternate path..."
    font_path = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    ren = renderer_create(trim(font_path), 16)
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
  ! First try fontconfig to find a font with Nerd Font icons (e.g., eza uses these)
  ! Try Nerd Font-specific devicons first (U+E5FF), then common icons (U+F07B)
  fallback_path = font_find_for_codepoint(int(z'E5FF'))  ! Nerd Font devicon
  if (len_trim(fallback_path) > 0) then
    call renderer_load_fallback_font(ren, trim(fallback_path))
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

  ! If fontconfig didn't work, try hardcoded paths
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

  ! Calculate terminal dimensions based on font metrics
  term_cols = win_width / CELL_WIDTH
  term_rows = win_height / CELL_HEIGHT
  prev_width = win_width
  prev_height = win_height

  ! Initialize terminal state and parser
  call terminal_init(term, term_rows, term_cols)
  call parser_init(parser)

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

  ! Main event loop
  do while (.not. window_should_close(win) .and. pty_is_alive(pty))
    ! Check for window resize
    call window_get_size(win, win_width, win_height)
    if (win_width /= prev_width .or. win_height /= prev_height) then
      prev_width = win_width
      prev_height = win_height

      ! Update projection matrix
      call renderer_set_projection(ren, win_width, win_height)

      ! Calculate new terminal size and notify PTY and terminal
      new_cols = win_width / CELL_WIDTH
      new_rows = win_height / CELL_HEIGHT
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

    ! Clear screen with dark gray background
    call glClearColor(0.1, 0.1, 0.12, 1.0)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Render terminal buffer
    call renderer_begin(ren)

    scr => terminal_active_screen(term)
    scroll_offset = terminal_get_scroll_offset(term)

    ! Allocate/resize scrollback line buffer if needed
    if (.not. allocated(sb_line)) then
      allocate(sb_line(term_cols))
    else if (size(sb_line) /= term_cols) then
      deallocate(sb_line)
      allocate(sb_line(term_cols))
    end if

    do row = 1, scr%rows
      y = real(row) * CELL_HEIGHT

      ! Determine if this row shows scrollback or screen content
      sb_offset = scroll_offset - row + 1

      if (sb_offset > 0 .and. sb_offset <= terminal_get_scrollback_count(term)) then
        ! This row shows scrollback content
        call terminal_get_scrollback_line(term, sb_offset - 1, sb_line, term_cols)
        do col = 1, min(term_cols, scr%cols)
          cell = sb_line(col)
          if (cell%codepoint /= 32) then
            x = real(col - 1) * CELL_WIDTH
            r = real(cell%fg%r) / 255.0
            g = real(cell%fg%g) / 255.0
            b = real(cell%fg%b) / 255.0
            call renderer_draw_char(ren, x, y, cell%codepoint, r, g, b, 1.0)
          end if
        end do
      else
        ! This row shows screen content
        screen_row = row - scroll_offset
        if (screen_row >= 1 .and. screen_row <= scr%rows) then
          do col = 1, scr%cols
            cell = screen_get_cell(scr, screen_row, col)
            if (cell%codepoint /= 32) then
              x = real(col - 1) * CELL_WIDTH
              r = real(cell%fg%r) / 255.0
              g = real(cell%fg%g) / 255.0
              b = real(cell%fg%b) / 255.0
              call renderer_draw_char(ren, x, y, cell%codepoint, r, g, b, 1.0)
            end if
          end do
        end if
      end if
    end do

    ! Draw cursor if visible (only when not scrolled back)
    if (term%cursor%visible .and. scroll_offset == 0) then
      x = real(term%cursor%col - 1) * CELL_WIDTH
      y = real(term%cursor%row) * CELL_HEIGHT
      ! Draw cursor as underscore character for visibility
      call renderer_draw_char(ren, x, y, 95, 0.7, 0.7, 0.7, 1.0)  ! '_'
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
