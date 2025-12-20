program fortty
  use window_mod
  use gl_bindings
  use renderer_mod
  implicit none

  type(window_t) :: win
  type(renderer_t) :: ren
  integer :: win_width, win_height
  character(len=256) :: font_path

  ! Window dimensions
  win_width = 800
  win_height = 600

  ! Create window with OpenGL context
  win = window_create(win_width, win_height, "fortty")

  ! Font path - try common system locations
  font_path = "/usr/share/fonts/TTF/DejaVuSansMono.ttf"

  ! Create renderer with font
  ren = renderer_create(trim(font_path), 24)
  if (.not. ren%initialized) then
    print *, "Warning: Could not load font, trying alternate path..."
    font_path = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    ren = renderer_create(trim(font_path), 24)
  end if

  if (.not. ren%initialized) then
    print *, "Error: Could not initialize renderer"
    print *, "Please ensure DejaVu Sans Mono font is installed"
    call window_destroy(win)
    stop 1
  end if

  ! Set up projection matrix
  call renderer_set_projection(ren, win_width, win_height)

  ! Main event loop
  do while (.not. window_should_close(win))
    ! Clear screen with dark gray background
    call glClearColor(0.1, 0.1, 0.12, 1.0)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Begin new frame
    call renderer_begin(ren)

    ! Draw test text
    call renderer_draw_string(ren, 50.0, 100.0, "Hello, Fortran!", &
                              1.0, 1.0, 1.0, 1.0)

    call renderer_draw_string(ren, 50.0, 150.0, "Terminal emulator in progress...", &
                              0.7, 0.7, 0.7, 1.0)

    ! Flush to GPU
    call renderer_flush(ren)

    ! Swap buffers and poll events
    call window_swap_buffers(win)
    call window_poll_events()
  end do

  ! Cleanup
  call renderer_destroy(ren)
  call window_destroy(win)

end program fortty
