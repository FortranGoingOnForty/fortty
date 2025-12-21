module toml_parser_mod
  implicit none
  private

  public :: toml_file_t
  public :: toml_open, toml_close
  public :: toml_get_string, toml_get_integer, toml_get_real, toml_get_logical

  integer, parameter :: MAX_LINE_LEN = 512
  integer, parameter :: MAX_KEYS = 128

  ! Key-value pair storage
  type :: toml_entry_t
    character(len=64) :: section = ''
    character(len=64) :: key = ''
    character(len=256) :: value = ''
  end type toml_entry_t

  type :: toml_file_t
    type(toml_entry_t) :: entries(MAX_KEYS)
    integer :: count = 0
    logical :: loaded = .false.
  end type toml_file_t

contains

  ! Open and parse a TOML file
  function toml_open(path) result(tf)
    character(len=*), intent(in) :: path
    type(toml_file_t) :: tf
    integer :: unit_num, ios
    character(len=MAX_LINE_LEN) :: line
    character(len=64) :: current_section
    logical :: file_exists

    tf%count = 0
    tf%loaded = .false.
    current_section = ''

    ! Check if file exists
    inquire(file=path, exist=file_exists)
    if (.not. file_exists) return

    ! Open file
    open(newunit=unit_num, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return

    ! Parse line by line
    do
      read(unit_num, '(A)', iostat=ios) line
      if (ios /= 0) exit

      call parse_line(tf, line, current_section)
    end do

    close(unit_num)
    tf%loaded = .true.

  end function toml_open

  ! Close/cleanup TOML file (nothing to do for now)
  subroutine toml_close(tf)
    type(toml_file_t), intent(inout) :: tf
    tf%count = 0
    tf%loaded = .false.
  end subroutine toml_close

  ! Parse a single line
  subroutine parse_line(tf, line, current_section)
    type(toml_file_t), intent(inout) :: tf
    character(len=*), intent(in) :: line
    character(len=64), intent(inout) :: current_section
    character(len=MAX_LINE_LEN) :: trimmed
    integer :: eq_pos, end_pos

    trimmed = adjustl(line)

    ! Skip empty lines
    if (len_trim(trimmed) == 0) return

    ! Skip comments
    if (trimmed(1:1) == '#') return

    ! Check for section header [section]
    if (trimmed(1:1) == '[') then
      end_pos = index(trimmed, ']')
      if (end_pos > 2) then
        current_section = trimmed(2:end_pos-1)
      end if
      return
    end if

    ! Parse key = value
    eq_pos = index(trimmed, '=')
    if (eq_pos > 1) then
      if (tf%count >= MAX_KEYS) return

      tf%count = tf%count + 1
      tf%entries(tf%count)%section = current_section
      tf%entries(tf%count)%key = adjustl(trimmed(1:eq_pos-1))
      ! Trim trailing spaces from key
      tf%entries(tf%count)%key = trim(tf%entries(tf%count)%key)

      ! Parse value (skip leading spaces)
      tf%entries(tf%count)%value = adjustl(trimmed(eq_pos+1:))
      call parse_value(tf%entries(tf%count)%value)
    end if

  end subroutine parse_line

  ! Clean up a value (remove quotes, handle types)
  subroutine parse_value(value)
    character(len=*), intent(inout) :: value
    integer :: len_val

    value = adjustl(value)

    ! Remove inline comments FIRST (before removing quotes)
    ! This correctly handles "#FFFFFF" as a quoted string, not a comment
    call remove_inline_comment(value)

    len_val = len_trim(value)
    if (len_val == 0) return

    ! Remove surrounding double quotes for strings
    if (value(1:1) == '"' .and. len_val >= 2) then
      if (value(len_val:len_val) == '"') then
        value = value(2:len_val-1)
      end if
    end if

  end subroutine parse_value

  ! Remove inline comments from value
  subroutine remove_inline_comment(value)
    character(len=*), intent(inout) :: value
    integer :: i
    logical :: in_string

    in_string = .false.
    do i = 1, len_trim(value)
      if (value(i:i) == '"') then
        in_string = .not. in_string
      else if (value(i:i) == '#' .and. .not. in_string) then
        value = value(1:i-1)
        value = trim(value)
        return
      end if
    end do
  end subroutine remove_inline_comment

  ! Get a string value from the TOML file
  function toml_get_string(tf, section, key, default) result(val)
    type(toml_file_t), intent(in) :: tf
    character(len=*), intent(in) :: section, key
    character(len=*), intent(in) :: default
    character(len=256) :: val
    integer :: i

    val = default

    do i = 1, tf%count
      if (trim(tf%entries(i)%section) == trim(section) .and. &
          trim(tf%entries(i)%key) == trim(key)) then
        val = tf%entries(i)%value
        return
      end if
    end do

  end function toml_get_string

  ! Get an integer value from the TOML file
  function toml_get_integer(tf, section, key, default) result(val)
    type(toml_file_t), intent(in) :: tf
    character(len=*), intent(in) :: section, key
    integer, intent(in) :: default
    integer :: val
    character(len=256) :: str_val
    integer :: ios

    val = default
    str_val = toml_get_string(tf, section, key, '')

    if (len_trim(str_val) > 0) then
      read(str_val, *, iostat=ios) val
      if (ios /= 0) val = default
    end if

  end function toml_get_integer

  ! Get a real value from the TOML file
  function toml_get_real(tf, section, key, default) result(val)
    type(toml_file_t), intent(in) :: tf
    character(len=*), intent(in) :: section, key
    real, intent(in) :: default
    real :: val
    character(len=256) :: str_val
    integer :: ios

    val = default
    str_val = toml_get_string(tf, section, key, '')

    if (len_trim(str_val) > 0) then
      read(str_val, *, iostat=ios) val
      if (ios /= 0) val = default
    end if

  end function toml_get_real

  ! Get a logical value from the TOML file
  function toml_get_logical(tf, section, key, default) result(val)
    type(toml_file_t), intent(in) :: tf
    character(len=*), intent(in) :: section, key
    logical, intent(in) :: default
    logical :: val
    character(len=256) :: str_val

    val = default
    str_val = toml_get_string(tf, section, key, '')

    if (len_trim(str_val) > 0) then
      select case (trim(adjustl(str_val)))
        case ('true', 'True', 'TRUE', 'yes', 'Yes', 'YES', '1')
          val = .true.
        case ('false', 'False', 'FALSE', 'no', 'No', 'NO', '0')
          val = .false.
        case default
          val = default
      end select
    end if

  end function toml_get_logical

end module toml_parser_mod
