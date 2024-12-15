module day2415_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type object_t
    integer :: pos(2)
    character(len=1) :: typ
  contains
    procedure :: canmove => object_canmove
  end type

  type warehouse_t
    character(len=1), allocatable :: map(:,:)
    character(len=:), allocatable :: orders
    type(object_t), allocatable :: objs(:)
    integer :: orders_pos=0
    integer :: id_robot
    logical :: ispart2=.false.
  end type

  character(len=*), parameter :: ch_orders='^>v<'
  integer, parameter, dimension(2,4) :: dirs = reshape( &
    [-1, 0, 0, 1, 1, 0, 0, -1], [2,4])

contains

  subroutine day2415(file)
    character(len=*), intent(in) :: file

    type(warehouse_t) :: wh, wh2
    integer :: ans1, ans2

    ! Part 1
    wh = warehouse_fromfile(file, .false.)
    do while(wh%orders_pos < len_trim(wh%orders))
      call move(wh)
    end do
    !call warehouse_print(wh)
    ans1 = warehouse_score(wh)

    ! Part 2
    wh2 = warehouse_fromfile(file, .true.)
    do while(wh2%orders_pos < len_trim(wh2%orders))
      call move(wh2)
    end do
    !call warehouse_print(wh2)
    ans2 = warehouse_score(wh2)

    print '("Ans 15/1 ",i0,l2)', ans1, ans1==1490942
    print '("Ans 15/2 ",i0,l2)', ans2, ans2==1519202
  end subroutine day2415


  function warehouse_score(this) result(n)
    class(warehouse_t), intent(in) :: this
    integer :: n

    integer :: i

    n = 0
    do i=1, size(this%objs)
      if (this%objs(i)%typ=='O') then
        associate(ij=>this%objs(i)%pos)
          n = n + 100*(ij(1)-1) + ij(2)-1
        end associate
      end if
    end do
  end function warehouse_score


  function warehouse_fromfile(file, ispart2) result(this)
    character(len=*), intent(in) :: file
    logical, intent(in) :: ispart2
    type(warehouse_t) :: this

    type(string_t), allocatable :: lines(:)
    integer :: ncols, nrows, i, j, k

    ! read the map
    lines = read_strings(file)
    ncols = 0
    do i=1,size(lines)
      if (ncols==0) ncols = len_trim(lines(i)%str)
      if (ncols/=len_trim(lines(i)%str)) exit
    end do
    nrows = i-1
    allocate(this%map(nrows, ncols))
    do i=1, nrows
      do j=1, ncols
        this%map(i,j) = lines(i)%str(j:j)
      end do
    end do

    ! read the orders
    j = 0
    do i=nrows+2, size(lines)
      j = j + len_trim(lines(i)%str)
    end do
    allocate(character(len=j) :: this%orders)
    j = 0
    do i=nrows+2, size(lines)
      this%orders(j+1:j+len_trim(lines(i)%str)) = lines(i)%str
      j = j + len_trim(lines(i)%str)
    end do

    ! find the robot
    if (count(this%map=='@')/=1) error stop 'could not find one robot'

    ! construct objects
    this%id_robot = 0
    associate(nobj=>size(this%map)-count(this%map=='.'))
      allocate(this%objs(nobj))
      k = 0
      do i=1,size(this%map,1)
        do j=1,size(this%map,2)
          select case(this%map(i,j))
          case('.')
            continue
          case('#','O','@')
            k = k+1
            this%objs(k)%pos = [i,j]
            this%objs(k)%typ = this%map(i,j)
            if (this%map(i,j)=='@') this%id_robot = k
          case default
            error stop 'invalid object'
          end select
        end do
      end do
      if (k/=nobj) error stop 'some objects were not found'
    end associate
    if (this%id_robot<1) error stop 'robot not found'

    ! extend for part2
    this%ispart2 = ispart2
    if (ispart2) then
      deallocate(this%map)
      allocate(this%map(nrows, 2*ncols))

      do i=1, size(this%objs)
        associate(x=>this%objs(i)%pos(2))
          x = 2*(x-1)+1
        end associate
      end do
      call warehouse_reconstructmap(this)
    end if
  end function warehouse_fromfile


  subroutine warehouse_reconstructmap(this)
    class(warehouse_t), intent(inout) :: this

    integer :: i

    this%map = '.'
    do i=1, size(this%objs)
      associate(row=>this%objs(i)%pos(1), col=>this%objs(i)%pos(2))
        this%map(row,col) = this%objs(i)%typ 
        if (this%ispart2) then
          select case (this%objs(i)%typ)
          case('O')
            this%map(row,col) = '['
            this%map(row,col+1) = ']'
          case('@')
            continue
          case('#')
            this%map(row,col+1) = this%objs(i)%typ 
          case default
            error stop 'unknown object type'
          end select
        end if
      end associate
    end do
  end subroutine warehouse_reconstructmap


  subroutine warehouse_print(this)
    class(warehouse_t), intent(in) :: this

    integer :: i, j
    do i=1, size(this%map,1)
      print '(*(a))', (this%map(i,j), j=1, size(this%map,2))
    end do
  end subroutine warehouse_print


  subroutine move(this)
    class(warehouse_t), intent(inout) :: this

    integer :: idir
    integer :: tot_crates, tot_walls, tot_free
    logical :: moved

    ! sums used later to validate the movement
    tot_free = count(this%map=='.')
    tot_walls = count(this%map=='#')
    tot_crates = count(this%map=='O') + count(this%map=='[') + count(this%map==']')
    if (size(this%map)-1 /= tot_free+tot_walls+tot_crates) then
        print *, size(this%map), tot_free, tot_walls, tot_crates
        error stop 'invalid counts'
    end if

    ! get movement order
    this%orders_pos = this%orders_pos + 1
    if (this%orders_pos>len_trim(this%orders)) error stop 'end of orders reached'
    idir = scan(ch_orders, this%orders(this%orders_pos:this%orders_pos))
    if (idir==0) error stop 'invalid order'

    ! move robot
    call object_move(this%id_robot, this%objs, idir, moved, this%ispart2)
    if (moved) then
      call warehouse_reconstructmap(this)
    end if

    ! checks that nothing broke
    if ( tot_free /= count(this%map=='.') .or.  &
         tot_walls /= count(this%map=='#') .or. &
         tot_crates /= &
         count(this%map=='O') + count(this%map=='[')+count(this%map==']'))  &
        error stop 'something wrong after move'
  end subroutine move


  recursive subroutine object_move(id, objs, idir, moved, ispart2)
    integer, intent(in) :: id
    type(object_t), intent(inout) :: objs(:)
    integer, intent(in) :: idir
    logical, intent(out) :: moved
    logical, intent(in) :: ispart2

    integer :: k, k2
    logical :: moved1

    if (objs(id)%canmove(objs, idir, ispart2)) then
      moved = .true.
      associate(newpos => objs(id)%pos+dirs(:,idir))
        ! find another object at the new position and ask it to move
        k = check_pos(objs, newpos, ispart2)
        if (k/=0 .and. k/=id) then
          if (objs(k)%typ /= 'O') &
              & error stop 'asking to move something that can not move (1)'
          call object_move(k, objs, idir, moved1, ispart2)
        end if

        ! also move object for the ']' part
        if (ispart2 .and. objs(id)%typ=='O') then
          k2 = check_pos(objs, newpos+[0,1], ispart2)
          if (k2/=0 .and. k2/=id) then
            if (objs(k2)%typ /= 'O') &
                & error stop 'asking to move something that can not move (2)'
            call object_move(k2, objs, idir, moved1, ispart2)
          end if
        end if

        ! now move the object itself
        objs(id)%pos = objs(id)%pos + dirs(:,idir)
      end associate
    else
      moved = .false.
    end if
  end subroutine object_move


  recursive function object_canmove(this, objs, idir, ispart2) result(can_move)
    class(object_t), intent(in) :: this
    type(object_t), intent(in) :: objs(:)
    integer, intent(in) :: idir
    logical, intent(in) :: ispart2
    logical :: can_move

    integer :: k, k2

    select case(this%typ)
    case('#')
      can_move = .false.
    case('@')
      k = check_pos(objs, this%pos+dirs(:,idir), ispart2)
      if (k==0) then
        can_move = .true.
      else
        can_move = object_canmove(objs(k), objs, idir, ispart2)
      end if
    case('O')
      select case(ch_orders(idir:idir))
      case('<')
        k  = check_pos(objs, this%pos+dirs(:,idir), ispart2)
        if (k==0) then
          can_move = .true.
        else
          can_move = object_canmove(objs(k), objs, idir, ispart2)
        end if
      case('>')
        if (ispart2) then
          k  = check_pos(objs, this%pos+dirs(:,idir)+[0,1], ispart2)
        else
          k  = check_pos(objs, this%pos+dirs(:,idir), ispart2)
        end if
        if (k==0) then
          can_move = .true.
        else
          can_move = object_canmove(objs(k), objs, idir, ispart2)
        end if
      case('^','v')
        k  = check_pos(objs, this%pos+dirs(:,idir), ispart2)
        if (ispart2) then
          k2 = check_pos(objs, this%pos+dirs(:,idir)+[0,1], ispart2)
        else
          k2 = 0
        end if
        if (k==0 .and. k2==0) then
          can_move = .true.
        else if (k==0) then
          can_move = object_canmove(objs(k2), objs, idir, ispart2)
        else if (k2==0) then
          can_move = object_canmove(objs(k), objs, idir, ispart2)
        else
          can_move = object_canmove(objs(k), objs, idir, ispart2) .and. &
            object_canmove(objs(k2), objs, idir, ispart2)
        end if
      end select
    case default
      error stop 'could not recognize object type'
    end select
  end function object_canmove


  function check_pos(hay, pos, ispart2) result(needle)
    class(object_t), intent(in) :: hay(:)
    integer, intent(in) :: pos(2)
    logical, intent(in) :: ispart2
    integer :: needle

    integer :: i

    do i=1, size(hay)
      if (all(hay(i)%pos==pos)) exit
      if (ispart2) then
        if (all(hay(i)%pos+[0,1]==pos) .and. hay(i)%typ/='@') exit
      end if
    end do
    if (i==size(hay)+1) then
      needle = 0 ! no object at that position
    else
      needle = i
    end if
  end function check_pos

end module day2415_mod