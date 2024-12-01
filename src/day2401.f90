module day2401_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public :: day2401

contains

  subroutine day2401(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ans1, ans2, i
    integer, allocatable :: aa(:), bb(:)

    lines = read_strings(file)
    allocate(aa(size(lines)), bb(size(lines)))
    do i=1, size(lines)
      read(lines(i)%str,*) aa(i), bb(i)
    end do

    ! Part 1
    call sort(aa)
    call sort(bb)
    ans1 = sum(abs(aa-bb))
    print '("Ans 01/1 ",i0,l2)', ans1, ans1==3569916

    ! Part 2
    ans2 = 0
    do i=1, size(lines)
      ans2 = ans2 + aa(i)*count(bb==aa(i))
    end do
    print '("Ans 01/2 ",i0,l2)', ans2, ans2==26407426

  end subroutine day2401


  subroutine sort(arr)
    integer, intent(inout) :: arr(:)
!
! Insertion sort algorithm
!
    integer :: i, j, key

    do i=2, size(arr)
      key = arr(i)
      j = i-1
      do 
        if (j<1) exit
        if (arr(j)<=key) exit
        arr(j+1) = arr(j)
        j = j-1
      end do
      arr(j+1) = key
    end do
  end subroutine sort

end module day2401_mod