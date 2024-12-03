module day2403_mod
  use parse_mod, only : string_t, read_strings
  implicit none
contains

  logical function isdigit(ch)
    character(len=1), intent(in) :: ch
    isdigit = iachar(ch)>=iachar('0') .and. iachar(ch)<=iachar('9')
  end function


  subroutine read_number(str, pos, num, issuccess)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: pos
    integer, intent(out) :: num
    logical, intent(out) :: issuccess

    integer :: i

    ! how many digits are there?
    i = pos
    do
      if (.not. isdigit(str(i:i))) exit
      i = i + 1
    end do
    if (i-pos > 0 ) then
      read(str(pos:i-1),*) num
      issuccess = .true.
    else
      issuccess = .false.
    end if
    pos = i
  end subroutine


  subroutine read_text(str, pos, text, issuccess)
    character(len=*), intent(in) :: str, text
    integer, intent(inout) :: pos
    logical, intent(out) :: issuccess

    integer :: i, j

    i = pos
    j = 1
    do
      if (j>len(text)) exit
      if (i>len(str)) exit
      if (str(i:i) /= text(j:j)) exit
      i = i + 1
      j = j + 1
    end do
    issuccess = j==len(text)+1
    if (issuccess) pos = pos + len(text) - 1
  end subroutine


  subroutine machine(str, pos, ans, do_on, is_part1)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: pos
    integer, intent(inout) :: ans
    logical, intent(inout) :: do_on
    logical, intent(in) :: is_part1

    integer :: state, i, num1, num2
    logical :: number_ok, text_ok
    integer, parameter :: S_M=0, S_FN=4, S_C=5, S_SN=6, S_RB=7, S_OK=8

    i = pos
    state = S_M
    do
      if (pos>len(str)) exit
!print *, 'machine =',str(pos:pos), state
      select case(state)
      case(S_M)
        if (str(pos:pos)=='m') then
          call read_text(str, pos, 'mul(', text_ok)
          if (text_ok) state = S_FN
        else if (str(pos:pos)=='d') then
          call read_text(str, pos, 'do()', text_ok)
          if (text_ok) then
            do_on = .true.
          else
            call read_text(str, pos, "don't()", text_ok)
            if (text_ok) then
              do_on = .false.
            end if
          end if
        end if
      case(S_FN)
        call read_number(str, pos, num1, number_ok)
        if (number_ok) then
          state = S_C
          pos = pos-1
        else
          state = S_M
        end if
      case(S_C)
        if (str(pos:pos)==',') then
          state = S_SN
        else
          state = S_M
        end if
      case(S_SN)
        call read_number(str, pos, num2, number_ok)
        if (number_ok) then
          state = S_RB
          pos = pos-1
        else
          state = S_M
        end if
      case(S_RB)
        if (str(pos:pos)==')') then
          state = S_M
          if (do_on) then
            ans = ans + num1*num2
          else
            if (is_part1) then
              ans = ans + num1*num2
            else
            end if
          end if
        else
          state = S_M
        end if
      case default
        error stop 'invalid state'
      end select
      pos = pos + 1
    end do

  end subroutine


  subroutine day2403(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2
    type(string_t), allocatable :: lines(:)

    integer :: pos, i
    logical :: do_on

    lines = read_strings(file)

    ! Part 1
    do_on = .true.
    ans1 = 0
    do i=1,size(lines)
      pos = 1
      call machine(lines(i)%str, pos, ans1, do_on, .true.)
    end do
    print '("Ans 03/1 ",i0,l2)', ans1, ans1==191183308

    ! Part 2
    do_on = .true.
    ans2 = 0
    do i=1,size(lines)
      pos = 1
      call machine(lines(i)%str, pos, ans2, do_on, .false.)
    end do
    print '("Ans 03/2 ",i0,l2)', ans2, ans2==92082041
  end subroutine

end module day2403_mod