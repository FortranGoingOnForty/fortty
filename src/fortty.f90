program fortty
  use window_mod
  use gl_bindings
  implicit none

  type(window_t) :: win

  ! Create window with OpenGL context
  win = window_create(800, 600, "fortty")

  ! Main event loop
  do while (.not. window_should_close(win))
    ! Clear screen with dark gray background
    call glClearColor(0.1, 0.1, 0.12, 1.0)
    call glClear(GL_COLOR_BUFFER_BIT)

    ! Swap buffers and poll events
    call window_swap_buffers(win)
    call window_poll_events()
  end do

  ! Cleanup
  call window_destroy(win)

end program fortty
