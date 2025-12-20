module wcwidth_mod
  implicit none
  private

  public :: codepoint_width

contains

  ! Determine display width of a Unicode codepoint
  ! Returns 0 for zero-width, 1 for normal, 2 for wide characters
  function codepoint_width(cp) result(w)
    integer, intent(in) :: cp
    integer :: w

    w = 1  ! Default width

    ! Zero-width characters
    if (cp == 0) then
      w = 0
      return
    end if

    ! C0/C1 control characters are zero-width
    if (cp < 32 .or. (cp >= 127 .and. cp < 160)) then
      w = 0
      return
    end if

    ! Combining characters (selected ranges)
    ! Combining Diacritical Marks: U+0300-U+036F
    if (cp >= int(z'0300') .and. cp <= int(z'036F')) then
      w = 0
      return
    end if

    ! Combining Diacritical Marks Extended: U+1AB0-U+1AFF
    if (cp >= int(z'1AB0') .and. cp <= int(z'1AFF')) then
      w = 0
      return
    end if

    ! Combining Diacritical Marks Supplement: U+1DC0-U+1DFF
    if (cp >= int(z'1DC0') .and. cp <= int(z'1DFF')) then
      w = 0
      return
    end if

    ! Combining Diacritical Marks for Symbols: U+20D0-U+20FF
    if (cp >= int(z'20D0') .and. cp <= int(z'20FF')) then
      w = 0
      return
    end if

    ! Variation Selectors: U+FE00-U+FE0F
    if (cp >= int(z'FE00') .and. cp <= int(z'FE0F')) then
      w = 0
      return
    end if

    ! Zero Width Joiner/Non-Joiner: U+200B-U+200D
    if (cp >= int(z'200B') .and. cp <= int(z'200D')) then
      w = 0
      return
    end if

    ! Soft Hyphen: U+00AD
    if (cp == int(z'00AD')) then
      w = 0
      return
    end if

    ! Wide character ranges (East Asian Width: Wide or Fullwidth)

    ! CJK Unified Ideographs: U+4E00-U+9FFF
    if (cp >= int(z'4E00') .and. cp <= int(z'9FFF')) then
      w = 2
      return
    end if

    ! CJK Unified Ideographs Extension A: U+3400-U+4DBF
    if (cp >= int(z'3400') .and. cp <= int(z'4DBF')) then
      w = 2
      return
    end if

    ! CJK Unified Ideographs Extension B: U+20000-U+2A6DF
    if (cp >= int(z'20000') .and. cp <= int(z'2A6DF')) then
      w = 2
      return
    end if

    ! CJK Compatibility Ideographs: U+F900-U+FAFF
    if (cp >= int(z'F900') .and. cp <= int(z'FAFF')) then
      w = 2
      return
    end if

    ! CJK Compatibility Ideographs Supplement: U+2F800-U+2FA1F
    if (cp >= int(z'2F800') .and. cp <= int(z'2FA1F')) then
      w = 2
      return
    end if

    ! Hangul Syllables: U+AC00-U+D7AF
    if (cp >= int(z'AC00') .and. cp <= int(z'D7AF')) then
      w = 2
      return
    end if

    ! Hangul Jamo: U+1100-U+11FF
    if (cp >= int(z'1100') .and. cp <= int(z'11FF')) then
      w = 2
      return
    end if

    ! Hangul Compatibility Jamo: U+3130-U+318F
    if (cp >= int(z'3130') .and. cp <= int(z'318F')) then
      w = 2
      return
    end if

    ! Hangul Jamo Extended-A: U+A960-U+A97F
    if (cp >= int(z'A960') .and. cp <= int(z'A97F')) then
      w = 2
      return
    end if

    ! Hangul Jamo Extended-B: U+D7B0-U+D7FF
    if (cp >= int(z'D7B0') .and. cp <= int(z'D7FF')) then
      w = 2
      return
    end if

    ! CJK Radicals Supplement: U+2E80-U+2EFF
    if (cp >= int(z'2E80') .and. cp <= int(z'2EFF')) then
      w = 2
      return
    end if

    ! Kangxi Radicals: U+2F00-U+2FDF
    if (cp >= int(z'2F00') .and. cp <= int(z'2FDF')) then
      w = 2
      return
    end if

    ! CJK Symbols and Punctuation: U+3000-U+303F
    if (cp >= int(z'3000') .and. cp <= int(z'303F')) then
      w = 2
      return
    end if

    ! Hiragana: U+3040-U+309F
    if (cp >= int(z'3040') .and. cp <= int(z'309F')) then
      w = 2
      return
    end if

    ! Katakana: U+30A0-U+30FF
    if (cp >= int(z'30A0') .and. cp <= int(z'30FF')) then
      w = 2
      return
    end if

    ! Katakana Phonetic Extensions: U+31F0-U+31FF
    if (cp >= int(z'31F0') .and. cp <= int(z'31FF')) then
      w = 2
      return
    end if

    ! Bopomofo: U+3100-U+312F
    if (cp >= int(z'3100') .and. cp <= int(z'312F')) then
      w = 2
      return
    end if

    ! Bopomofo Extended: U+31A0-U+31BF
    if (cp >= int(z'31A0') .and. cp <= int(z'31BF')) then
      w = 2
      return
    end if

    ! CJK Strokes: U+31C0-U+31EF
    if (cp >= int(z'31C0') .and. cp <= int(z'31EF')) then
      w = 2
      return
    end if

    ! Enclosed CJK Letters and Months: U+3200-U+32FF
    if (cp >= int(z'3200') .and. cp <= int(z'32FF')) then
      w = 2
      return
    end if

    ! CJK Compatibility: U+3300-U+33FF
    if (cp >= int(z'3300') .and. cp <= int(z'33FF')) then
      w = 2
      return
    end if

    ! Fullwidth ASCII: U+FF01-U+FF60
    if (cp >= int(z'FF01') .and. cp <= int(z'FF60')) then
      w = 2
      return
    end if

    ! Fullwidth Punctuation: U+FFE0-U+FFE6
    if (cp >= int(z'FFE0') .and. cp <= int(z'FFE6')) then
      w = 2
      return
    end if

    ! Halfwidth Katakana: U+FF65-U+FFDC (these are actually width 1)
    if (cp >= int(z'FF65') .and. cp <= int(z'FFDC')) then
      w = 1
      return
    end if

    ! Yi Syllables: U+A000-U+A48F
    if (cp >= int(z'A000') .and. cp <= int(z'A48F')) then
      w = 2
      return
    end if

    ! Yi Radicals: U+A490-U+A4CF
    if (cp >= int(z'A490') .and. cp <= int(z'A4CF')) then
      w = 2
      return
    end if

    ! Emoji (most are wide)
    ! Miscellaneous Symbols and Pictographs: U+1F300-U+1F5FF
    if (cp >= int(z'1F300') .and. cp <= int(z'1F5FF')) then
      w = 2
      return
    end if

    ! Emoticons: U+1F600-U+1F64F
    if (cp >= int(z'1F600') .and. cp <= int(z'1F64F')) then
      w = 2
      return
    end if

    ! Transport and Map Symbols: U+1F680-U+1F6FF
    if (cp >= int(z'1F680') .and. cp <= int(z'1F6FF')) then
      w = 2
      return
    end if

    ! Supplemental Symbols and Pictographs: U+1F900-U+1F9FF
    if (cp >= int(z'1F900') .and. cp <= int(z'1F9FF')) then
      w = 2
      return
    end if

    ! Symbols and Pictographs Extended-A: U+1FA00-U+1FA6F
    if (cp >= int(z'1FA00') .and. cp <= int(z'1FA6F')) then
      w = 2
      return
    end if

    ! Symbols and Pictographs Extended-B: U+1FA70-U+1FAFF
    if (cp >= int(z'1FA70') .and. cp <= int(z'1FAFF')) then
      w = 2
      return
    end if

    ! Default: width 1
    w = 1
  end function codepoint_width

end module wcwidth_mod
