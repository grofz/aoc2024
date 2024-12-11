module day2411_mod
  use iso_fortran_env, only : i8 => int64
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type darr_t
    integer(i8), allocatable :: aa(:)
    integer :: n
  contains
    procedure :: add => darr_add
  end type
  interface darr_t
    module procedure darr_fromfile
  end interface

contains

  subroutine day2411(file)
    character(len=*), intent(in) :: file

    type(darr_t) :: stones
    integer, parameter :: NTESTS1 = 25
    integer :: i
    integer(i8) :: ans1, ans2

    stones = darr_t(file)
    print '(*(i0,1x))', stones%aa(1:stones%n)
    do i=1, NTESTS1
      call make_blink(stones)
      print *, i, stones%n
      if (i==NTESTS1) ans1 = stones%n
    end do

    print '("Ans 11/1 ",i0,l2)', ans1, ans1==182081
  end subroutine day2411


  subroutine make_blink(this)
    class(darr_t), intent(inout) :: this

    integer :: i
    integer(i8) :: a, b

    do i=1, this%n
      associate(num=>this%aa(i))
        if (num==0) then
          num = 1
print *, '0 --> 1'
        else if (mod(ndigits(num),2)==0) then
          call split_number(num, a, b)
print *, num, ' --> ',a,' , ',b
          num = a
          call this%add(b)
        else
print *, num, ' --> ', num*2024
          num = num * 2024
        end if
      end associate
    end do
print *
  end subroutine


  function ndigits(ab) result(d)
    integer(i8), intent(in) :: ab
    integer :: d

    character(len=20) :: sab
    write(sab,'(i0)') ab
    d = len_trim(sab)
  end function ndigits


  subroutine split_number(ab, a, b)
    integer(i8), intent(in) :: ab
    integer(i8), intent(out) :: a, b

    character(len=20) :: sab
    integer :: n

    write(sab,'(i0)') ab
    n = len_trim(sab)
    if (mod(n,2)/=0) error stop 'number does not have even digits'
    read(sab(1:n/2),*) a
    read(sab(n/2+1:),*) b
  end subroutine split_number


  function darr_fromfile(file) result(new)
    character(len=*), intent(in) :: file
    type(darr_t) :: new

    type(string_t), allocatable :: line(:), tokens(:)
    integer :: i
    integer(i8) :: added

    line = read_strings(file)
    if (size(line)/=1) error stop 'just one line expected'
    call split(line(1)%str, ' ', tokens)
    do i=1, size(tokens)
      read(tokens(i)%str,*) added
      call new%add(added)
    end do
  end function darr_fromfile


  subroutine darr_add(this, added)
    class(darr_t), intent(inout) :: this
    integer(i8), intent(in) :: added

    integer(i8), allocatable :: wrk(:)

    ! make sure there is a space in the array for the added number
    if (.not. allocated(this%aa)) then
      allocate(this%aa(8))
      this%n = 0
    else if (this%n == size(this%aa)) then
      allocate(wrk(2*size(this%aa)))
      wrk(1:this%n) = this%aa
      call move_alloc(wrk, this%aa)
    end if

    ! add the number at the end of the array
    this%n = this%n + 1
    this%aa(this%n) = added
  end subroutine darr_add


end module day2411_mod