module day2413_mod
  use iso_fortran_env, only : i8 => int64, dp => real64
  use parse_mod, only : string_t, read_strings
  implicit none

  integer(i8), parameter, dimension(2) :: COSTS = [3,1]

  type machine_t
    integer(i8) :: p(2)       ! prize position
    integer(i8) :: ab(2,2)    ! position matrix [ax, bx; ay, by]
    integer(i8) :: n(2)=-1    ! counter of presses [na, nb]
    integer(i8) :: cost_to_win=0
  end type

contains

  subroutine day2413(file)
    character(len=*), intent(in) :: file

    integer(i8) :: ans1, ans2
    integer :: i
    type(string_t), allocatable :: lines(:)
    type(machine_t), allocatable :: machines(:)

    lines = read_strings(file)
    if (mod(size(lines)+1,4)/=0) error stop 'unexpected number of lines'
    allocate(machines((size(lines)+1)/4))

    ! Part 1
    do i=1, size(machines)
      call machine_from_lines(machines(i), lines, 4*(i-1)+1)
      call solve_by_algebra(machines(i))
    end do
    ans1 = sum(machines(:)%cost_to_win)

    ! Part 2
    do i=1,size(machines)
      machines(i)%p = machines(i)%p + 10000000000000_i8
      call solve_by_algebra(machines(i))
    end do
    ans2 = sum(machines(:)%cost_to_win)

    print '("Ans 13/1 ",i0,l2)', ans1, ans1==37297
    print '("Ans 13/2 ",i0,l2)', ans2, ans2==83197086729371_i8
  end subroutine day2413


  subroutine machine_from_lines(this, lines, first_line)
    type(machine_t), intent(out) :: this
    type(string_t), intent(in) :: lines(:)
    integer, intent(in) :: first_line
!
! Parse input
!
    integer :: i, j
    character(len=*), parameter :: key1a = "Button A: X+"
    character(len=*), parameter :: key1b = "Button B: X+"
    character(len=*), parameter :: key2  = ", Y+"
    character(len=*), parameter :: key3 = "Prize: X="
    character(len=*), parameter :: key4  = ", Y="

    associate(f=>lines(first_line)%str)
      i = index(f, key1a)
      j = index(f, key2)
      if (i==0 .or. j==0) error stop 'could not parse first line'
      read(f(i+len(key1a):j-1),*) this%ab(1,1)
      read(f(j+len(key2):),*) this%ab(2,1)
    end associate
    associate(f=>lines(first_line+1)%str)
      i = index(f, key1b)
      j = index(f, key2)
      if (i==0 .or. j==0) error stop 'could not parse second line'
      read(f(i+len(key1b):j-1),*) this%ab(1,2)
      read(f(j+len(key2):),*) this%ab(2,2)
    end associate
    associate(f=>lines(first_line+2)%str)
      i = index(f, key3)
      j = index(f, key4)
      if (i==0 .or. j==0) error stop 'could not parse third line'
      read(f(i+len(key3):j-1),*) this%p(1)
      read(f(j+len(key4):),*) this%p(2)
    end associate
  end subroutine machine_from_lines


  subroutine machine_show(this)
    class(machine_t), intent(in) :: this

    100 format("|",i0,"| = |",i0,1x,i0,"| |",i0,"|")
    print 100, this%p(1), this%ab(1,:), this%n(1)
    print 100, this%p(2), this%ab(2,:), this%n(2)
    print *
  end subroutine


  subroutine solve_by_algebra(this)
    class(machine_t), intent(inout) :: this
 
    real(dp) :: a(2,2), b(2), x(2), ainv(2,2), adet
     
    ! Solving set of linear equations
    !
    !   | px |     | ax bx |   | na |
    !   |    |  =  |       | * |    |
    !   | py |     | ay by |   | nb |
    !
    ! for an unknown vector [na, nb]
    !
    a = real(this%ab, dp)
    b = real(this%p, dp)
    adet = a(1,1)*a(2,2) - a(1,2)*a(2,1)
    if (abs(adet)<10*spacing(0.0_dp)) error stop 'singular matrix'
    ainv(1,1) = a(2,2)
    ainv(2,2) = a(1,1)
    ainv(1,2) = -a(1,2)
    ainv(2,1) = -a(2,1)
    ainv = ainv/adet
    b = matmul(ainv, b)

    ! Convert solution to nearest integer and check if equation
    ! still holds. Invalidate if not.
    this%n = nint(b,i8)
    if (.not. all(matmul(this%ab, this%n)==this%p)) this%n = -1

    ! Calculate the cost
    if (any(this%n==-1)) then
      this%cost_to_win = 0
    else
      this%cost_to_win = dot_product(COSTS, this%n)
    end if
  end subroutine solve_by_algebra

end module day2413_mod