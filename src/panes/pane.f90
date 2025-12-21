module pane_mod
  use terminal_mod
  use pty_mod
  use parser_mod
  implicit none
  private

  public :: pane_t
  public :: pane_init, pane_destroy
  public :: pane_resize, pane_set_viewport

  ! Maximum panes per tab
  integer, parameter, public :: MAX_PANES = 16

  type :: pane_t
    type(terminal_t) :: term
    type(pty_t) :: pty
    type(parser_t) :: parser

    ! Viewport/layout (in pixels)
    integer :: x = 0           ! Left edge
    integer :: y = 0           ! Top edge
    integer :: width = 800     ! Width in pixels
    integer :: height = 600    ! Height in pixels

    ! Terminal dimensions (in cells)
    integer :: rows = 24
    integer :: cols = 80

    ! Split tracking for layout recalculation
    integer :: split_type = 0     ! 0=none, 1=vertical, 2=horizontal
    integer :: parent_idx = 0     ! Index of parent pane (0 if root)
    real :: split_ratio = 0.5     ! Position in parent (0.0-1.0)

    logical :: active = .false.
  end type pane_t

contains

  ! Initialize a new pane with terminal, PTY, and parser
  subroutine pane_init(pane, rows, cols)
    type(pane_t), intent(inout) :: pane
    integer, intent(in) :: rows, cols

    pane%rows = rows
    pane%cols = cols

    ! Initialize terminal
    call terminal_init(pane%term, rows, cols)

    ! Initialize parser
    call parser_init(pane%parser)

    ! Open PTY with shell
    pane%pty = pty_open("", rows, cols)

    pane%active = .false.
    pane%split_type = 0
    pane%parent_idx = 0
    pane%split_ratio = 0.5
  end subroutine pane_init

  ! Destroy a pane and clean up resources
  subroutine pane_destroy(pane)
    type(pane_t), intent(inout) :: pane

    call pty_close(pane%pty)
    call terminal_destroy(pane%term)

    pane%active = .false.
    pane%x = 0
    pane%y = 0
    pane%width = 0
    pane%height = 0
    pane%rows = 0
    pane%cols = 0
  end subroutine pane_destroy

  ! Resize a pane's terminal and PTY
  subroutine pane_resize(pane, new_rows, new_cols)
    type(pane_t), intent(inout) :: pane
    integer, intent(in) :: new_rows, new_cols

    if (new_rows /= pane%rows .or. new_cols /= pane%cols) then
      pane%rows = new_rows
      pane%cols = new_cols
      call pty_resize(pane%pty, new_rows, new_cols)
      call terminal_resize(pane%term, new_rows, new_cols)
    end if
  end subroutine pane_resize

  ! Set a pane's viewport and recalculate terminal dimensions
  subroutine pane_set_viewport(pane, x, y, w, h, cell_w, cell_h)
    type(pane_t), intent(inout) :: pane
    integer, intent(in) :: x, y, w, h
    integer, intent(in) :: cell_w, cell_h
    integer :: new_rows, new_cols

    pane%x = x
    pane%y = y
    pane%width = w
    pane%height = h

    ! Calculate terminal dimensions from pixel size
    new_cols = w / cell_w
    new_rows = h / cell_h

    ! Ensure minimum dimensions
    if (new_cols < 2) new_cols = 2
    if (new_rows < 2) new_rows = 2

    ! Resize if dimensions changed
    call pane_resize(pane, new_rows, new_cols)
  end subroutine pane_set_viewport

end module pane_mod
