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

  ! Keys at the numeric keypad hardcoded
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

  ! Keys at the directional keypad hardcoded
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

  ! A keypad
  type keypad_t
    type(key_t), allocatable :: keys(:)
  contains
    procedure :: path => keypad_path
    procedure :: validpath => keypad_validpath
  end type

  ! There are just two keypad kinds
  integer, parameter :: NUMERIC_KEYPAD_KIND=1, DIRAL_KEYPAD_KIND=2

contains

  pure function hash(ch) result(ind) 
    character(len=1), intent(in) :: ch
    integer :: ind
!
! Convert a character to a an index used in the memoization table
!
    ind = scan(HASH_TABLE, ch)
    if (ind==0) error stop 'could not obtain index for the character on input'
  end function hash


  pure function new_keypad(keypad_kind) result(this)
    integer, intent(in) :: keypad_kind
    type(keypad_t) :: this

    select case(keypad_kind)
    case(NUMERIC_KEYPAD_KIND)
      this%keys = &
      &   [KEY_7, KEY_4, KEY_1, KEY_X, KEY_8, KEY_5, KEY_2, KEY_0, KEY_9, KEY_6, KEY_3, KEY_A]
    case(DIRAL_KEYPAD_KIND)
      this%keys = &
      &   [KEY_DX, KEY_LEFT, KEY_UP, KEY_DOWN, KEY_DA, KEY_RIGHT]
    end select
  end function new_keypad


  function keypad_path(this, edge) result(res)
    class(keypad_t), intent(in) :: this
    character(len=2), intent(in) :: edge
    type(string_t), allocatable :: res(:)
!
! Return one or two paths to move finger from edge(1:1) to edge(2:2)
!
    integer :: bp(2), ep(2), pos, i, j
    type(string_t) :: res0(2)
    logical :: res0_valid(2)

!   pos = findloc(this%keys%lab, edge(1:1), dim=1)
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
    res0_valid(1) = this%validpath(res0(1)%str, bp, ep)
    res0_valid(2) = this%validpath(res0(2)%str, bp, ep)

    ! avoid duplicit results
    if (res0(1)%str==res0(2)%str) res0_valid(2) = .false.

    allocate(res(count(res0_valid)))
    j = 0
    do i=1, 2
      if (.not. res0_valid(i)) cycle
      j = j + 1
      res(j) = string_t(res0(i)%str)
    end do
  end function keypad_path


  pure function keypad_validpath(this, str, bp, ep) result(yes)
    class(keypad_t), intent(in) :: this
    character(len=*), intent(in) :: str
    integer, intent(in) :: bp(2), ep(2)
    logical :: yes
!
! Used to check if the path goes over the "off-limits" position on the keypad
!
    integer :: xp(2), i, j

    yes = .true.
    xp = bp
    do i=1, len(str)+1
      do j=1, size(this%keys)
        if (all(this%keys(j)%pos==xp)) exit
      end do
      if (j==size(this%keys)+1) error stop 'could not find index of the xp'
      if (this%keys(j)%offlimit) yes = .false.
      if (i>len(str)) exit
      xp = xp + DIRS(:, index(DIRS_CH, str(i:i)))
    end do
    if (.not. all(xp==ep)) error stop 'ending position does not match'
  end function keypad_validpath


  recursive subroutine keystrokes(seq, uplevel, maxlevel, keypads, nkeys, mem)
    character(len=2), intent(in) :: seq
    integer, intent(in) :: uplevel, maxlevel
    type(keypad_t), intent(in) :: keypads(:)
    integer(I8), intent(out) :: nkeys
    integer(I8), intent(inout), allocatable :: mem(:,:,:)
!
! Return the minimum number of keystrokes at level-1 keypad required
! to move the finger at the level-"uplevel" keypad from the position
! "seq(1:1)" to the position "seq(2:2)" followed by pressing the key at
! final position.
!
    integer(I8) :: nkeys0, nsum
    integer(I8), parameter :: UNKNOWN=-1
    integer :: ipath, i
    type(string_t), allocatable :: path(:)

    ! Moving finger at level-1 keypad is free, just press the button
    if (uplevel == 1) then
      nkeys = 1
      return ! no-recursion branch
    end if

    ! For the first call - set-up the memoization table
    if (.not. allocated(mem)) then
      allocate(mem(len(HASH_TABLE), len(HASH_TABLE), maxlevel), source=UNKNOWN)
    end if

    ! Check the memoization table first if we already know the result
    nkeys = mem(hash(seq(1:1)), hash(seq(2:2)), uplevel)
    if (nkeys /= UNKNOWN) return

    ! Plan the path on the level-"uplevel" keypad, return the sequence of
    ! key-strokes that has to be pressed at the current level keypad.
    if (uplevel==maxlevel) then
      path = keypads(NUMERIC_KEYPAD_KIND)%path(seq)
    else
      path = keypads(DIRAL_KEYPAD_KIND)%path(seq)
    end if

    ! Then recursively evaluate the path sequence to obtain the result
    ! There can be up to two possible paths, we should try both of them
    nkeys = huge(nkeys)
    do ipath=1, size(path)
      ! add key "A" at the start and the end of the path
      path(ipath) = string_t('A'//path(ipath)%str//'A')
      nsum = 0
      do i=1, len(path(ipath)%str)-1
        call keystrokes(path(ipath)%str(i:i+1), uplevel-1, maxlevel, keypads, nkeys0, mem)
        nsum = nsum + nkeys0
      end do
      if (nsum < nkeys) nkeys = nsum
    end do

    ! Remember the result
    mem(hash(seq(1:1)), hash(seq(2:2)), uplevel) = nkeys
!print '("Up-level ",i0," sequence ",a,": ",i0," keystrokes")', uplevel, seq, nkeys
  end subroutine keystrokes


  subroutine day2421(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(keypad_t) :: keypads(2)
    integer :: iline, val, j, ipart
    integer(I8) :: nkeys, totkeys, ans(2)
    integer(I8), allocatable :: mem(:,:,:)
    character(len=1) :: pch
    integer, parameter, dimension(2) :: nlevels = [4, 27]

    keypads(NUMERIC_KEYPAD_KIND) = new_keypad(NUMERIC_KEYPAD_KIND)
    keypads(DIRAL_KEYPAD_KIND) = new_keypad(DIRAL_KEYPAD_KIND)
    lines = read_strings(file)

    ans = 0
    PART: do ipart = 1, 2
      if (allocated(mem)) deallocate(mem) ! clean memoization table

      LINE: do iline = 1, size(lines)
        associate(code=>lines(iline)%str)
          ! number of keystrokes to type the code
          totkeys = 0
          do j=1, len(code)
            if (j==1) then
              pch = 'A'
            else
              pch = code(j-1:j-1)
            end if
            call keystrokes(pch//code(j:j), nlevels(ipart), nlevels(ipart), keypads, nkeys, mem)
            totkeys = totkeys + nkeys
          end do

          ! code "complexity"
          read(code(1:3),*) val
          ans(ipart) = ans(ipart) + val * totkeys
        end associate
      end do LINE

    end do PART

    print '("Ans 21/1 ",i0,l2)', ans(1), ans(1)==213536
    print '("Ans 21/2 ",i0,l2)', ans(2), ans(2)==258369757013802_i8
  end subroutine day2421

end module day2421_mod