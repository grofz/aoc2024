module day2407_mod
  use iso_fortran_env, only : i8=>int64
  use parse_mod, only : string_t, read_strings, split
  use loop_mod, only : loop_iterator_t
  implicit none

  type expression_t
    integer(i8) :: result
    integer(i8), allocatable :: terms(:)
  end type

  interface operator(//)
    module procedure concat_numbers
  end interface

contains

  subroutine day2407(file)
    character(len=*), intent(in) :: file

    integer(i8) :: ans1, ans2
    type(expression_t), allocatable :: exprs(:)
    integer :: i

    exprs = expression_fromfile(file)
    ans1 = 0
    ans2 = 0
    write(*,*)
    do i=1, size(exprs)
      !print *, exprs(i)%res
      !print '(*(i0,1x),/)', exprs(i)%terms
!!    if (try_old(exprs(i), .false.)) then
      if (try(exprs(i), exprs(i)%terms(1), 1, .false.)) then
        ans1 = ans1 + exprs(i)%result
        ans2 = ans2 + exprs(i)%result
!!    else if (try_old(exprs(i), .true.)) then
      else if (try(exprs(i), exprs(i)%terms(1), 1, .true.)) then
        ans2 = ans2 + exprs(i)%result
      end if
      write(*,'(a)',advance='no') repeat(' ',70)//char(13)
      write(*,'(i3,1x,i18,i18,i18)',advance = 'no') i, exprs(i)%result, ans1, ans2
    end do
    write(*,*)

    print '("Ans 07/1 ",i0,l2)', ans1, ans1==7710205485870_i8
    print '("Ans 07/2 ",i0,l2)', ans2, ans2==20928985450275_i8
  end subroutine day2407


  recursive pure function try(expr, accval, i, is_part2) result(is_ok)
    class(expression_t), intent(in) :: expr
    integer(i8), intent(in) :: accval  ! accumulated value for terms 1..i
    integer, intent(in) :: i
    logical, intent(in) :: is_part2
    logical :: is_ok

    if (accval > expr%result) then
      ! overshoot, geting the given value is impossible
      is_ok = .false.
    else if (i == size(expr%terms)) then
      ! tip of the branch
      is_ok = accval == expr%result
    else if (try(expr, accval + expr%terms(i+1), i+1, is_part2)) then
      ! did the addition work?
      is_ok = .true.
    else if (try(expr, accval * expr%terms(i+1), i+1, is_part2)) then
      ! did the multiplication work?
      is_ok = .true.
    else if (is_part2) then
      if (try(expr, accval // expr%terms(i+1), i+1, is_part2)) then
        ! did the concatenation work?
        is_ok = .true.
      else
        ! any of three operations worked
        is_ok = .false.
      end if
    else
      ! any of two operations worked (in part1)
      is_ok = .false.
    end if
  end function try


  pure function concat_numbers(a, b) result(c)
    integer(i8), intent(in) :: a, b
    integer(i8) :: c
!
! Concatenate two numbers
!
    character(len=20) :: as, bs, cs

    write(as,'(i0)') a
    write(bs,'(i0)') b
    cs = trim(as)//trim(bs)
    read(cs,*) c
  end function concat_numbers


  function expression_fromfile(file) result(new)
    character(len=*), intent(in) :: file
    type(expression_t), allocatable :: new(:)
!
! Parse input
!
    type(string_t), allocatable :: lines(:), terms(:)
    integer :: i, j

    lines = read_strings(file)
    allocate(new(size(lines)))
    do i=1,size(lines)
      call split(lines(i)%str,' ',terms)

      associate(res=>terms(1)%str)
        if (res(len(res):len(res))/=':') error stop 'colon expected but not found'
        read(res(1:len(res)-1), *) new(i)%result
      end associate

      allocate(new(i)%terms(size(terms)-1))
      do j=1,size(terms)-1
        read(terms(j+1)%str, *) new(i)%terms(j)
      end do
    end do
  end function expression_fromfile


!
! old (not-optimized) implementation
!

  logical function try_old(this, is_part2) result (is_ok)
    class(expression_t), intent(in) :: this
    logical, intent(in) :: is_part2
!
! Loop over all possible combinations of the operators.
! It is not optimized.
!
    type(loop_iterator_t) :: iterator
    integer, allocatable :: lbs(:), ubs(:), sts(:)
    integer(i8) :: res

    associate(n=>size(this%terms)-1)
      allocate(lbs(n), sts(n), source=1)
      if (.not. is_part2) then
        allocate(ubs(n), source=2) ! addition, multiplication
      else
        allocate(ubs(n), source=3) ! addition, multiplication, concantenation
      end if
    end associate
    iterator = loop_iterator_t(lbs, ubs, sts)
    is_ok = .false.
    do while (iterator%has_next_element())
      res = eval(this, iterator%get_next_element())
      if (res == this%result) then ! valid answer
        is_ok = .true.
        exit
      end if
    end do
  end function try_old


  pure function eval(this, ops)
    class(expression_t), intent(in) :: this
    integer, intent(in) :: ops(:)
    integer(i8) :: eval

    integer :: i
    integer, parameter :: OP_ADD=1, OP_MUL=2, OP_CON=3

    eval = this%terms(1)
    do i=2, size(this%terms)
      select case(ops(i-1))
      case(OP_ADD)
        eval = eval + this%terms(i)
      case(OP_MUL)
        eval = eval * this%terms(i)
      case(OP_CON)
        eval = eval // this%terms(i)
      case default
        error stop 'invalid op'
      end select
    end do
  end function eval

end module day2407_mod