module day2421_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : I8 => int64
  implicit none

  ! For each key at any keypad...
  type key_t
    character(len=1) :: lab        ! label
    integer :: pos(2)              ! position [row, col]
    logical :: offlimit = .false.  ! finger may not pass over this "key"
  end type

  ! Keys at the numeric keypad
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

  ! Keys at the directional keypad
  type(key_t), parameter :: &
    KEY_DX    = key_t('X',[1,1],offlimit=.true.), &
    KEY_UP    = key_t('^',[1,2]), &
    KEY_DA    = key_t('A',[1,3]), &
    KEY_LEFT  = key_t('<',[2,1]), &
    KEY_DOWN  = key_t('v',[2,2]), &
    KEY_RIGHT = key_t('>',[2,3])

  ! Which direction the finger moves by pressing a particularly labeled key?
  integer, parameter, dimension(2,4) :: DIRS = reshape( &
      & [ -1,0, 0,-1, 1,0, 0,1], [2,4])
  character(len=*), parameter :: DIRS_CH='^<v>'

  ! Hash-table
  character(len=*), parameter :: HASH_TABLE = "^<v>A0123456789"

  ! A keypad (TODO: do we need this user-defined type?)
  type board_t
    type(key_t), allocatable :: keys(:)
  end type

  ! There are just two keypad kinds
  integer, parameter :: NUMERIC_KEYPAD_KIND=1, DIRAL_KEYPAD_KIND=2

contains

  pure function hash(ch) result(ind) 
    character(len=1), intent(in) :: ch
    integer :: ind

    ind = scan(HASH_TABLE, ch)
    if (ind==0) error stop 'could not find hash of a particular character'
  end function hash


  pure function numeric_keypad() result(this)
    type(board_t) :: this
    this%keys = [KEY_7, KEY_4, KEY_1, KEY_X, KEY_8, KEY_5, KEY_2, KEY_0, KEY_9, KEY_6, KEY_3, KEY_A]
  end function numeric_keypad


  pure function diral_keypad() result(this)
    type(board_t) :: this
    this%keys = [KEY_DX, KEY_LEFT, KEY_UP, KEY_DOWN, KEY_DA, KEY_RIGHT]
  end function diral_keypad


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


  recursive subroutine keystrokes(seq, uplevel, maxlevel, keypads, nkeys, mem)
    character(len=2), intent(in) :: seq
    integer, intent(in) :: uplevel, maxlevel
    type(board_t), intent(in) :: keypads(:)
    integer(I8), intent(out) :: nkeys
    integer(I8), intent(inout), allocatable :: mem(:,:,:)
!
! Return the minimum number of keystrokes on level-1 keypad required
! to move the finger at the level-"uplevel" keypad from the position
! "seq(1:1)" to the position "seq(2:2)" and  press key at final position.
!
    integer(I8) :: nkeys0, nkeys1
    integer :: keypad_kind, ipath, i
    type(string_t), allocatable :: path(:)
    integer(I8), parameter :: UNKNOWN=-1

    ! In the first call - set-up the memoization table
    if (.not. allocated(mem)) then
      allocate(mem(len(HASH_TABLE), len(HASH_TABLE), maxlevel), source=UNKNOWN)
    end if

    ! Moving finger at level-1 keypad is free, just press the button
    if (uplevel == 1) then
      nkeys = 1
      return
    end if

    ! Check the memoization table first if we know the result
    nkeys = mem(hash(seq(1:1)), hash(seq(2:2)), uplevel)
    if (nkeys /= UNKNOWN) return

print '("Up-level ",i0," sequence ",a,2(1x,i0))', uplevel, seq, hash(seq(1:1)), hash(seq(2:2))

    ! Plan the route on the level-"uplevel" keypad, return the sequence of
    ! key-strokes made on the current level keypad.
    if (uplevel==maxlevel) then
      keypad_kind = NUMERIC_KEYPAD_KIND
    else
      keypad_kind = DIRAL_KEYPAD_KIND
    end if
    path = move(keypads(keypad_kind), seq)

    ! Then recursively evaluate this sequence to obtain the result
    ! There can be up to two possible paths
    nkeys = huge(nkeys)
    do ipath=1, size(path)
      ! add key "A" at the start and the end of the path
      path(ipath) = string_t('A'//path(ipath)%str//'A')
      nkeys0 = 0
      do i=1, len(path(ipath)%str)-1
        call keystrokes(path(ipath)%str(i:i+1), uplevel-1, maxlevel, keypads, nkeys1, mem)
        nkeys0 = nkeys0 + nkeys1
      end do
      if (nkeys0 < nkeys) nkeys = nkeys0
    end do

    ! Remember the result
    mem(hash(seq(1:1)), hash(seq(2:2)), uplevel) = nkeys
  end subroutine keystrokes


  subroutine day2421(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(board_t) :: keypads(2)
    integer :: min_press, i, val, j, ipart
    integer(I8) :: nkeys, nkeys0, ans(2)
    integer(I8), allocatable :: mem(:,:,:)
    character(len=1) :: pch
    integer, parameter, dimension(2) :: nkeypads = [4, 27]

    keypads(NUMERIC_KEYPAD_KIND) = numeric_keypad()
    keypads(DIRAL_KEYPAD_KIND) = diral_keypad()
    lines = read_strings(file)

    ans = 0
    PART: do ipart=1,2
      if (allocated(mem)) deallocate(mem)
      do i=1, size(lines)
        read(lines(i)%str(1:3),*) val

        nkeys = 0
        do j=1, len(lines(i)%str)
          if (j==1) then
            pch = 'A'
          else
            pch = lines(i)%str(j-1:j-1)
          end if
        !call keystrokes(pch//lines(i)%str(j:j), 4, 4, keypads, nkeys0, mem)
        ! call keystrokes(pch//lines(i)%str(j:j), 27, 27, keypads, nkeys0, mem)
          call keystrokes(pch//lines(i)%str(j:j), nkeypads(ipart), nkeypads(ipart), keypads, nkeys0, mem)
          nkeys = nkeys + nkeys0
        end do

  !print '("Old method ",i0,"  New method ",i0)', min_press, nkeys

  !     ans1 = ans1 + val * min_press
  !     ans1 = ans1 + val * nkeys
        ans(ipart) = ans(ipart) + val * nkeys
  print *, lines(i)%str, val, nkeys
      end do
print *, 'hashed values =', count(mem/=-1)
    end do PART

    print '("Ans 21/1 ",i0,l2)', ans(1), ans(1)==213536
    print '("Ans 21/2 ",i0,l2)', ans(2), ans(2)==258369757013802_i8
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