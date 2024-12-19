module day2419_mod
  use parse_mod, only : read_strings, string_t, split
  use iso_fortran_env, only : i8 => int64
  implicit none

contains

  subroutine day2419(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), tpack(:)
    integer :: stte, i, ans1
    integer(i8) :: ans2, nways
    integer(i8), allocatable :: memo(:)

    lines = read_strings(file)
    call split(lines(1)%str, ',', tpack)
    do i=1,size(tpack)
      ! remove a space after the comma
      tpack(i) = string_t(trim(adjustl(tpack(i)%str)))
    end do
    if (lines(2)%str/='') error stop 'second line expected to be empty'

    ans1 = 0
    ans2 = 0_i8
    do i=3, size(lines)
      stte = 0 ! how many characters are covered
!!    print '(a)', lines(i)%str
      allocate(memo(0:len(lines(i)%str)), source=-1_i8)
      call cover_pattern(lines(i)%str, stte, tpack, nways, memo)
      deallocate(memo)
!!    print '(i0)', nways
      if (nways>0) ans1 = ans1 + 1
      ans2 = ans2 + nways
    end do

    print '("Ans 19/1 ",i0,l2)', ans1, ans1==276
    print '("Ans 19/2 ",i0,l2)', ans2, ans2==681226908011510_i8
  end subroutine day2419


  recursive subroutine cover_pattern(pattern, stte, tpack, nways, memo)
    character(len=*), intent(in) :: pattern
    integer, intent(in) :: stte
    type(string_t), intent(in) :: tpack(:)
    integer(i8), intent(out) :: nways
    integer(i8), intent(inout) :: memo(0:)

    integer :: i, stte_0
    integer(i8) :: nways_0

    if (memo(stte)>= 0) then
      ! use already known value
      nways = memo(stte)
      return
    end if

    if (stte==len(pattern)) then
      nways = 1
      memo(stte) = nways
      return
    end if

    nways = 0
    do i=1, size(tpack)
      ! ignore towels that are too long
      if (len(tpack(i)%str) > len(pattern)-stte) cycle

      ! ignore towels that do not match the start of pattern
      if (tpack(i)%str /= pattern(stte+1:stte+len(tpack(i)%str))) cycle

      ! recursively try the rest of pattern
      stte_0 = stte+len(tpack(i)%str)
      call cover_pattern(pattern, stte_0, tpack, nways_0, memo)

      ! collect all possible ways
      nways = nways + nways_0
    end do

    ! store result for the future
    memo(stte) = nways
  end subroutine cover_pattern

end module day2419_mod