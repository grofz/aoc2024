module day2411_mod
  use iso_fortran_env, only : i8 => int64
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type darr_t
    integer(i8), allocatable :: aa(:), cnt(:)
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
    integer, parameter :: PART_ONE = 25, PART_TWO = 75
    integer :: i
    integer(i8) :: ans1, ans2

    stones = darr_t(file)
    print '(*(i0,1x))', stones%aa(1:stones%n)
    do i=1, PART_TWO
      call make_blink(stones)
!print *, i, stones%n, sum(stones%cnt(1:stones%n))
!print '(*(i0,1x))', stones%aa(1:stones%n)
!print *
      if (i==PART_ONE) ans1 = sum(stones%cnt(1:stones%n))
      if (i==PART_TWO) ans2 = sum(stones%cnt(1:stones%n))
    end do

    print '("Ans 11/1 ",i0,l2)', ans1, ans1==182081
    print '("Ans 11/2 ",i0,l2)', ans2, ans2==216318908621637_i8
  end subroutine day2411


  subroutine make_blink(this)
    type(darr_t), intent(inout) :: this

    type(darr_t) :: this_upt

    integer :: i
    integer(i8) :: a, b

    do i=1, this%n
      associate(num=>this%aa(i), cnt=>this%cnt(i))
        if (num==0) then
          call this_upt%add(1_i8, cnt)
        else if (mod(ndigits(num),2)==0) then
          call split_number(num, a, b)
          call this_upt%add(a, cnt)
          call this_upt%add(b, cnt)
        else
          call this_upt%add(num*2024_i8, cnt)
        end if
      end associate
    end do
    this%n = this_upt%n
    call move_alloc(this_upt%aa, this%aa)
    call move_alloc(this_upt%cnt, this%cnt)
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
      call new%add(added,1_i8)
    end do
  end function darr_fromfile


  subroutine darr_add(this, add_aa, add_cnt)
    class(darr_t), intent(inout) :: this
    integer(i8), intent(in) :: add_aa, add_cnt

    integer(i8), allocatable :: wrk(:)
    integer :: i

    ! make sure there is a space in the array for the added number
    if (.not. allocated(this%aa)) then
      allocate(this%aa(4000))
      allocate(this%cnt(4000))
      this%n = 0
    else if (this%n == size(this%aa)) then
      allocate(wrk(2*this%n))
      wrk(1:this%n) = this%aa
      call move_alloc(wrk, this%aa)
      allocate(wrk(2*this%n))
      wrk(1:this%n) = this%cnt
      call move_alloc(wrk, this%cnt)
    end if

    ! look if item is already present
    do i=1, this%n
      if (this%aa(i)==add_aa) exit
    end do

    if (i==this%n+1) then
      ! add the number at the end of the array
      this%n = this%n + 1
      this%aa(this%n) = add_aa
      this%cnt(this%n) = add_cnt
    else
      ! just increase the number of items
      this%cnt(i) = this%cnt(i) + add_cnt
    end if
  end subroutine darr_add


end module day2411_mod
