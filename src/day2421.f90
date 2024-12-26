module day2421_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type key_t
    character(len=1) :: lab
    integer :: pos(2)
    logical :: offlimit = .false.
  end type

  type(key_t), parameter :: &
    KEY_7 = key_t('7',[1,1]), &
    KEY_8 = key_t('8',[1,2]), &
    KEY_9 = key_t('9',[1,3]), &
    KEY_4 = key_t('4',[2,1]), &
    KEY_5 = key_t('5',[2,2]), &
    KEY_6 = key_t('6',[2,3]), &
    KEY_1 = key_t('1',[3,1]), &
    KEY_2 = key_t('2',[3,2]), &
    KEY_3 = key_t('3',[3,3]), &
    KEY_X = key_t('X',[4,1],offlimit=.true.), &
    KEY_0 = key_t('0',[4,2]), &
    KEY_A = key_t('A',[4,3])

  type(key_t), parameter :: &
    KEY_DX    = key_t('X',[1,1],offlimit=.true.), &
    KEY_UP    = key_t('^',[1,2]), &
    KEY_DA    = key_t('A',[1,3]), &
    KEY_LEFT  = key_t('<',[2,1]), &
    KEY_DOWN  = key_t('v',[2,2]), &
    KEY_RIGHT = key_t('>',[2,3])

  integer, parameter, dimension(2,4) :: DIRS = reshape( &
      & [ -1,0, 0,-1, 1,0, 0,1], [2,4])
  character(len=*), parameter :: DIRS_CH='^<v>'

  type board_t
    type(key_t), allocatable :: keys(:)
  end type

contains

  pure function numeric_board() result(this)
    type(board_t) :: this
    this%keys = [KEY_7, KEY_4, KEY_1, KEY_X, KEY_8, KEY_5, KEY_2, KEY_0, KEY_9, KEY_6, KEY_3, KEY_A]
  end function numeric_board


  pure function diral_board() result(this)
    type(board_t) :: this
    this%keys = [KEY_DX, KEY_LEFT, KEY_UP, KEY_DOWN, KEY_DA, KEY_RIGHT]
  end function diral_board


  function instructions(this, str) result(res)
    class(board_t), intent(in) :: this
    character(len=*), intent(in) :: str
    type(string_t), allocatable :: res(:)

    character(len=1) :: pch
    type(string_t), allocatable :: res0(:), tmp(:)
    integer :: i, j

    allocate(res(1))
    res(1) = string_t('')

    do i=1, len(str)
      if (i==1) then
        pch = 'A'
      else 
        pch = str(i-1:i-1)
      end if
      res0 = move(this, pch//str(i:i))
      select case (size(res0))
      case(1)
        ! just append instructions at the end
        do j=1, size(res)
          res(j) = string_t(res(j)%str//res0(1)%str//'A')
        end do
      case(2)
        ! duplicate and just append instructions at the end
        allocate(tmp(size(res)*2))
        do j=1, size(res)
          tmp(j) = string_t(res(j)%str//res0(1)%str//'A')
          tmp(size(res)+j) = string_t(res(j)%str//res0(2)%str//'A')
        end do
        call move_alloc(tmp, res)
      case default
        error stop 'invalid branch'
      end select
    end do
  end function instructions


  function move(this, edge) result(res)
    class(board_t), intent(in) :: this
    character(len=2), intent(in) :: edge
    type(string_t), allocatable :: res(:)

    integer :: bp(2), ep(2), xp(2), pos, i, j
    type(string_t) :: res0(2)
    logical :: res0_valid(2)

   !pos = findloc(this%keys%lab, edge(1:1), dim=1)
    do pos=1, size(this%keys)
      if (this%keys(pos)%lab==edge(1:1)) exit
    end do
    if (pos==size(this%keys)+1) error stop 'could not locate key 1 on the board'
   !if (pos==0) error stop 'could not locate key 1 on the board'
    bp = this%keys(pos)%pos
   !pos = findloc(this%keys%lab, edge(2:2), dim=1)
    do pos=1, size(this%keys)
      if (this%keys(pos)%lab==edge(2:2)) exit
    end do
    if (pos==size(this%keys)+1) error stop 'could not locate key 2 on the board'
   !if (pos==0) error stop 'could not locate key 2 on the board'
    ep = this%keys(pos)%pos

    ! we return only one or two possible ways
    ! (1) move horizontally, then vertically
    ! (2) move vertically, then horizontally 
    associate (ncopy => abs(ep-bp))
      if (ep(2)>bp(2)) then
        res0(1)=string_t(repeat('>',ncopy(2)))
        res0(2)=string_t(repeat('>',ncopy(2)))
      else if (ep(2)<bp(2)) then
        res0(1)=string_t(repeat('<',ncopy(2)))
        res0(2)=string_t(repeat('<',ncopy(2)))
      else
        res0(1)=string_t('')
        res0(2)=string_t('')
      end if
      if (ep(1)>bp(1)) then
        res0(1)=string_t(res0(1)%str//repeat('v',ncopy(1)))
        res0(2)=string_t(repeat('v',ncopy(1))//res0(2)%str)
      else if (ep(1)<bp(1)) then
        res0(1)=string_t(res0(1)%str//repeat('^',ncopy(1)))
        res0(2)=string_t(repeat('^',ncopy(1))//res0(2)%str)
      else
        continue
      end if
    end associate

    ! now simulate the path to see if it goes over "offlimit" keys
    res0_valid(1) = valid_path(this, res0(1)%str, bp, ep)
    res0_valid(2) = valid_path(this, res0(2)%str, bp, ep)

    ! avoid duplicit results
    if (res0(1)%str==res0(2)%str) res0_valid(2) = .false.

    allocate(res(count(res0_valid)))
    j = 0
    do i=1, 2
      if (.not. res0_valid(i)) cycle
      j = j + 1
      res(j) = string_t(res0(i)%str)
    end do
  end function move


  function valid_path(this, str, bp, ep) result(yes)
    class(board_t), intent(in) :: this
    character(len=*), intent(in) :: str
    integer, intent(in) :: bp(2), ep(2)
    logical :: yes

    integer :: xp(2), i, j

    yes = .true.
    xp = bp
    do i=1, len(str)+1
      do j=1, size(this%keys)
        if (all(this%keys(j)%pos==xp)) exit
      end do
      if (j==size(this%keys)+1) error stop 'could not find index of the xp'
      if (this%keys(j)%offlimit) then
        yes = .false.
      end if
      if (i>len(str)) exit
      xp = xp + DIRS(:, index(DIRS_CH, str(i:i)))
    end do
    if (.not. all(xp==ep)) error stop 'ending position does not match'
  end function valid_path


  subroutine day2421(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(board_t) :: door, radiation, freezer
    integer :: min_press, i, val, ans1


    door = numeric_board()
    radiation = diral_board()
    freezer = diral_board()
    lines = read_strings(file)

    ans1 = 0
    do i=1, size(lines)
      read(lines(i)%str(1:3),*) val
      call process_code(lines(i)%str, door, radiation, freezer, min_press)
      ans1 = ans1 + val * min_press
print *, lines(i)%str, val, min_press
    end do

    print '("Ans 21/1 ",i0,l2)', ans1, ans1==213536
  end subroutine day2421


  subroutine process_code(inp, door, radiation, freezer, min_press)
    character(len=*), intent(in) :: inp
    type(board_t), intent(in) :: door, radiation, freezer
    integer, intent(out) :: min_press

    integer :: i, j, k
    type(string_t), allocatable :: paths_door(:), paths_radiation(:), paths_freezer(:)

    min_press = huge(min_press)
    paths_door = instructions(door, trim(inp))
    A: do i=1, size(paths_door)
!!    print '("door  ",a,1x,i0)', paths_door(i)%str, len(paths_door(i)%str)

      paths_radiation = instructions(radiation, paths_door(i)%str)
      B: do j=1, size(paths_radiation)
!!      print '("radiation  ",a,1x,i0)', paths_radiation(j)%str, len(paths_radiation(j)%str)

        paths_freezer = instructions(freezer, paths_radiation(j)%str)
        C: do k=1, size(paths_freezer)
!!        print '("freez  ",a,1x,i0)', paths_freezer(k)%str, len(paths_freezer(k)%str)
          if (len(paths_freezer(k)%str)<min_press) &
            min_press = len(paths_freezer(k)%str)
        end do C

!!      print *
      end do B

!!    print *, repeat('-',80)
!!    print *
    end do A
  end subroutine

end module day2421_mod