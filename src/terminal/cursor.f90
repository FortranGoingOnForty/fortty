module cursor_mod
  use cell_mod
  implicit none
  private

  public :: cursor_t
  public :: CURSOR_BLOCK, CURSOR_UNDERLINE, CURSOR_BAR

  ! Cursor styles
  integer, parameter :: CURSOR_BLOCK = 0
  integer, parameter :: CURSOR_UNDERLINE = 1
  integer, parameter :: CURSOR_BAR = 2

  type :: cursor_t
    integer :: row = 1              ! Current row (1-indexed)
    integer :: col = 1              ! Current column (1-indexed)
    logical :: visible = .true.     ! Cursor visibility
    integer :: style = CURSOR_BLOCK ! Cursor style
    logical :: blink = .true.       ! Cursor blink enabled
    type(color_t) :: fg             ! Current foreground color for drawing
    type(color_t) :: bg             ! Current background color for drawing
    integer :: attrs = 0            ! Current attributes for drawing
  end type cursor_t

end module cursor_mod
