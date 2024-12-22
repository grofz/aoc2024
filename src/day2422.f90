module day2422_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : I8 => int64

  type pseudo_generator_t
    integer(I8) :: seed
    integer(I8) :: cnt = 0, dif = -100
  end type
  interface pseudo_generator_t
    module procedure pseudo_generator_new
  end interface

contains

  subroutine day2422(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer(I8) :: ans1
    integer :: i, j
    type(pseudo_generator_t) :: generator
    integer, parameter :: P1_REPEAT = 2000

    ans1 = 0
    lines = read_strings(file)
    do i=1, size(lines)
      generator = pseudo_generator_t(lines(i)%str)
      do j=1, P1_REPEAT
        call harvest(generator)
print '(3(i3))', j, last_digit(generator), generator%dif
      end do
      ans1 = ans1 + generator%seed
print *
    end do

    print '("Ans 22/1 ",i0,l2)', ans1, ans1==14476723788_I8

  end subroutine day2422


  function pseudo_generator_new(str) result(this)
    character(len=*), intent(in) :: str
    type(pseudo_generator_t) :: this

    read(str, *) this%seed
    this%cnt = 0
  end function pseudo_generator_new


  subroutine harvest(this)
    class(pseudo_generator_t) :: this

    integer :: d0

    d0 = last_digit(this)
    call mix(this, this%seed*64_I8)
    call prune(this)
    call mix(this, this%seed/32_I8)
    call prune(this)
    call mix(this, this%seed*2048_I8)
    call prune(this)
    this%cnt = this%cnt+1
    this%dif = last_digit(this)-d0

  contains
    subroutine mix(t, b)
      class(pseudo_generator_t), intent(inout) :: t 
      integer(I8), intent(in) :: b

      t%seed = ieor(t%seed, b)
    end subroutine mix

    subroutine prune(t)
      class(pseudo_generator_t), intent(inout) :: t 
      t%seed = modulo(t%seed, 16777216_I8)
    end subroutine prune
  end subroutine harvest


  function last_digit(this) result(d)
    class(pseudo_generator_t), intent(in) :: this
    integer :: d

    d = int(modulo(this%seed, 10_I8))
  end function last_digit

end module day2422_mod