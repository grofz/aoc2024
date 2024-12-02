module day2402_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none
  private
  public :: day2402

  type report_t
    integer, allocatable :: arr(:), dif(:)
  contains
    procedure :: safe_p1 => report_issafe
    procedure :: safe_p2 => report_issafe2
  end type
  interface report_t
    module procedure report_new, report_remove
  end interface

contains

  function report_new(str) result(new)
    character(len=*), intent(in) :: str
    type(report_t) :: new

    type(string_t), allocatable :: items(:)
    integer :: i, j

    call split(str, ' ', items)
    allocate(new % arr(size(items)))
    allocate(new % dif(size(items)-1))
    do i=1, size(items)
      read(items(i)%str,*) new%arr(i) 
    end do
    do j=1, size(items)-1
      new%dif(j) = new%arr(j+1)-new%arr(j)
    end do
  end function


  function report_remove(this, irem) result(new)
    class(report_t), intent(in) :: this
    integer, intent(in) :: irem
    type(report_t) :: new

    integer :: i, j

    allocate(new%arr(size(this%arr)-1))
    allocate(new%dif(size(this%arr)-2))

    j = 0
    do i=1, size(this%arr)
      if (i == irem) cycle
      j = j+1
      new%arr(j) = this%arr(i)
    end do
    do i=1, size(new%arr)-1
      new%dif(i) = new%arr(i+1)-new%arr(i)
    end do
  end function


  logical function report_issafe(this) result(issafe)
    class(report_t), intent(in) :: this

    logical :: ismonotonous, isclose

    ismonotonous = all(this%dif>0) .or. all(this%dif<0)
    isclose = all(abs(this%dif)>=1) .and. all(abs(this%dif)<=3)
    issafe = ismonotonous .and. isclose
  end function


  logical function report_issafe2(this) result(issafe)
    class(report_t), intent(in) :: this
!
! Remove one number from the report and then check for "safety" of the reduced
! report
!
    type(report_t) :: reduced
    integer :: i

    issafe = .false.
    do i=1, size(this%arr)
      reduced = report_t(this, i)
      if (reduced%safe_p1()) then
        issafe = .true.
        exit
      end if
    end do
  end function


  subroutine day2402(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(report_t), allocatable :: reports(:)
    integer :: ans1, ans2, i

    lines = read_strings(file)
    allocate(reports(size(lines)))
    do i=1, size(lines)
      reports(i) = report_t(lines(i)%str)
    end do

    ans1 = 0
    ans2 = 0
    do i=1, size(reports)
      if (reports(i)%safe_p1()) ans1 = ans1+1
      if (reports(i)%safe_p2()) ans2 = ans2+1
    end do
    print '("Ans 02/1 ",i0,l2)', ans1, ans1==442
    print '("Ans 02/2 ",i0,l2)', ans2, ans2==493
  end subroutine day2402

end module day2402_mod