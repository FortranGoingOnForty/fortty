program fortty
  use window_mod
  use gl_bindings
  use renderer_mod
  use pty_mod
  implicit none

  type(window_t) :: win
  type(renderer_t) :: ren
  type(pty_t) :: pty
  integer :: win_width, win_height
  integer :: prev_width, prev_height
  integer :: term_rows, term_cols
  integer :: new_rows, new_cols
  character(len=256) :: font_path
  character(len=4096) :: pty_buffer
  integer :: nbytes
  integer, parameter :: CELL_WIDTH = 10   ! Approximate char width
  integer, parameter :: CELL_HEIGHT = 20  ! Approximate line height

  ! Window dimensions
  win_width = 800
  win_height = 600

  ! Create window with OpenGL context
  win = window_create(win_width, win_height, "fortty")

  ! Font path - try common system locations
  font_path = "/usr/share/fonts/TTF/DejaVuSansMono.ttf"

  ! Create renderer with font
  ren = renderer_create(trim(font_path), 16)
  if (.not. ren%initialized) then
    print *, "Warning: Could not load font, trying alternate path..."
    font_path = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    ren = renderer_create(trim(font_path), 16)
  end if

  if (.not. ren%initialized) then
    print *, "Error: Could not initialize renderer"
    print *, "Please ensure DejaVu Sans Mono font is installed"
    call window_destroy(win)
    stop 1
  end if

  ! Set up projection matrix
  call renderer_set_projection(ren, win_width, win_height)

  ! Calculate terminal dimensions based on font metrics
  term_cols = win_width / CELL_WIDTH
  term_rows = win_height / CELL_HEIGHT
  prev_width = win_width
  prev_height = win_height

  ! Open PTY with shell
  pty = pty_open("", term_rows, term_cols)  ! Empty string = use $SHELL

  if (.not. pty%active) then
    print *, "Error: Could not open PTY"
    call renderer_destroy(ren)
    call window_destroy(win)
    stop 1
  end if

  print *, "PTY opened successfully"
  print *, "Terminal size:", term_cols, "x", term_rows

  ! Main event loop
  do while (.not. window_should_close(win) .and. pty_is_alive(pty))
    ! Check for window resize
    call window_get_size(win, win_width, win_height)
    if (win_width /= prev_width .or. win_height /= prev_height) then
      prev_width = win_width
      prev_height = win_height

      ! Update projection matrix
      call renderer_set_projection(ren, win_width, win_height)

      ! Calculate new terminal size and notify PTY
      new_cols = win_width / CELL_WIDTH
      new_rows = win_height / CELL_HEIGHT
      if (new_cols /= term_cols .or. new_rows /= term_rows) then
        term_cols = new_cols
        term_rows = new_rows
        call pty_resize(pty, term_rows, term_cols)
      end if
    end if

    ! Read from PTY (non-blocking)
    nbytes = pty_read(pty, pty_buffer, 4096)
    if (nbytes > 0) then
      ! For now, just print to stdout (Phase 4 will render to screen)
      write(*,'(A)', advance='no') pty_buffer(1:nbytes)
    end if

    ! Clear screen with dark gray background
    call glClearColor(0.1, 0.1, 0.12, 1.0)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Begin new frame
    call renderer_begin(ren)

    ! Draw status text
    call renderer_draw_string(ren, 10.0, 30.0, "fortty - Shell connected", &
                              0.0, 1.0, 0.0, 1.0)

    call renderer_draw_string(ren, 10.0, 60.0, "(Shell output goes to stdout for now)", &
                              0.5, 0.5, 0.5, 1.0)

    ! Flush to GPU
    call renderer_flush(ren)

    ! Swap buffers and poll events
    call window_swap_buffers(win)
    call window_poll_events()
  end do

  print *, ""
  print *, "Shell exited or window closed"

  ! Cleanup
  call pty_close(pty)
  call renderer_destroy(ren)
  call window_destroy(win)

end program fortty
