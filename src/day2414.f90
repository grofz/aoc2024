module day2414_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : i8 => real64
  implicit none

  type robot_t
    integer :: p(2), v(2)
  end type
  interface robot_t
    module procedure robot_from_str
  end interface

 !integer, parameter :: MAP_SIZE(2) = [11, 7]     ! sample inputs
  integer, parameter :: MAP_SIZE(2) = [101, 103]

contains

  subroutine day2414(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(robot_t), allocatable :: robots(:)

    integer :: i, nquad(4), ans1, ans2
    integer(i8) :: emin = huge(emin)
    logical :: tree_found, to_draw

    lines = read_strings(file)
    allocate(robots(size(lines)))
    do i=1, size(lines)
      robots(i) = robot_t(lines(i)%str)
    end do

    to_draw = .false.
    ans2 = -1
    ans1 = -1
    do i=1, product(MAP_SIZE)
      call robot_move(robots)
      if (to_draw) print *, i
      call plot_positions(robots, nquad, to_draw, tree_found)
      if (tree_found .and. ans2==-1) ans2 = i
      if (i==100) ans1 = product(nquad)
!     if (ans1>0 .and. ans2>0) exit
associate(e=>entropy(robots))
  if (e<=emin) then
    emin = e
!call plot_positions(robots, nquad, .true., tree_found)
!    print *, i, emin
  end if
end associate
    end do
    print '("Ans 14/1 ",i0,l2)', ans1, ans1==228457125
    print '("Ans 14/2 ",i0,l2)', ans2, ans2==6493
  end subroutine day2414


  !
  ! Parsing input
  !
  type(robot_t) function robot_from_str(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: toks(:)

    call split(str, ' ', toks)
    if (size(toks)/=2) error stop 'two terms expected'
    new%p = parse_vec(toks(1)%str, 'p=')
    new%v = parse_vec(toks(2)%str, 'v=')
  end function robot_from_str


  function parse_vec(str, pattern) result(x)
    character(len=*), intent(in) :: str, pattern
    integer :: x(2)

    character(len=*), parameter :: delim=','
    integer :: i, j

    i = index(str, pattern)
    if (i==0) error stop 'pattern not found'
    j = index(str, delim)
    if (j==0) error stop 'delimiter not found'
    read(str(i+len_trim(pattern):j-1),*) x(1)
    read(str(j+len_trim(delim):),*) x(2)
  end function parse_vec


  !
  ! Count number of robots
  !
  subroutine plot_positions(robots, nquad, to_print, is_tree)
    type(robot_t), intent(in) :: robots(:)
    integer, intent(out) :: nquad(4)
    logical, intent(in) :: to_print
    logical, intent(out) :: is_tree

    character(len=1) :: ch
    integer, dimension(MAP_SIZE(2), MAP_SIZE(1)) :: map
    integer :: i, j, mi, mj

    mi = size(map,1)/2+1
    mj = size(map,2)/2+1
    map = 0
    do i=1, size(robots)
      associate(px=>robots(i)%p(1), py=>robots(i)%p(2))
        if (px<0 .or. py<0 .or. px>MAP_SIZE(1)-1 .or. py>MAP_SIZE(2)-1) &
            & error stop 'invalid robot position'
        map(py+1, px+1) = map(py+1, px+1) + 1
      end associate
    end do

    ! test for the tree is ridiculously simple
    is_tree = maxval(map)<=1

    ! count number of robots in the quadrants
    nquad(1) = sum(map(1:mi-1, 1:mj-1))
    nquad(2) = sum(map(1:mi-1, mj+1:))
    nquad(3) = sum(map(mi+1:,  1:mj-1))
    nquad(4) = sum(map(mi+1:,  mj+1:))

    ! print robot positions
    if (to_print) then
      write(*,*)
      do i=1, size(map,1)
        do j=1,size(map,2)
          write(ch, '(i1)') map(i,j)
          if (map(i,j)==0) ch='.'
          write(*,'(a)',advance='no') ch
        end do
        write(*,*)
      end do
    end if

  end subroutine plot_positions


  !
  ! Robot movement
  !
  elemental subroutine robot_move(this)
    class(robot_t), intent(inout) :: this

    this%p(1) = modulo(this%p(1) + this%v(1), MAP_SIZE(1))
    this%p(2) = modulo(this%p(2) + this%v(2), MAP_SIZE(2))
  end subroutine robot_move


  function entropy(robots) result(e)
    type(robot_t), intent(in) :: robots(:)
    integer(i8) :: e

    integer :: i, j

    e = 0
    do i=1, size(robots)-1
      do j=i+1, size(robots)
        e = e + sum(abs(robots(i)%p-robots(j)%p)**2)
      end do
    end do

  end function entropy

end module day2414_mod