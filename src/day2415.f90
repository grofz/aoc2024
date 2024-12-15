module day2415_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type object_t
    integer :: pos(2)
    character(len=1) :: typ
  end type

  type warehouse_t
    character(len=1), allocatable :: map(:,:)
    character(len=:), allocatable :: orders
    type(object_t), allocatable :: objs(:)
    integer :: orders_pos=0
    integer :: pos(2)
  end type

  character(len=*), parameter :: ch_orders='^>v<'
  integer, parameter, dimension(2,4) :: dirs = reshape( &
    [-1, 0, 0, 1, 1, 0, 0, -1], [2,4])

contains

  subroutine day2415(file)
    character(len=*), intent(in) :: file

    type(warehouse_t) :: wh
    integer :: ans1, ans2

    wh = warehouse_fromfile(file)
    call warehouse_print(wh)
print *, 'objects = ', size(wh%objs)
    do while(wh%orders_pos < len_trim(wh%orders))
      call move(wh)
      !call warehouse_print(wh)
    end do
    call warehouse_print(wh)
    ans1 = warehouse_score(wh)

    print '("Ans 15/1 ",i0,l2)', ans1, ans1==1490942
    print '("Ans 15/2 ",i0,l2)', ans2, ans2==1
  end subroutine day2415


  function warehouse_score(this) result(n)
    class(warehouse_t), intent(in) :: this
    integer :: n

    integer :: i, j

    n = 0
    do i=1, size(this%map,1)
      do j=1, size(this%map,2)
        if (this%map(i,j)=='O') then
          n = n + 100*(i-1) + j-1
        end if
      end do
    end do
  end function warehouse_score


  function warehouse_fromfile(file) result(this)
    character(len=*), intent(in) :: file
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
    this%pos = findloc(this%map, '@')

    ! construct objects
    associate(nobj=>size(this%map)-1-count(this%map=='.'))
      allocate(this%objs(nobj))
      k = 0
      do i=1,size(this%map,1)
        do j=1,size(this%map,2)
          select case(this%map(i,j))
          case('@', '.')
            continue
          case('#','O')
            k = k+1
            this%objs(k)%pos = [i,j]
            this%objs(k)%typ = this%map(i,j)
          case default
            error stop 'invalid object'
          end select
        end do
      end do
      if (k/=nobj) error stop 'some objects were not found'
    end associate
  end function warehouse_fromfile


  subroutine warehouse_print(this)
    class(warehouse_t), intent(in) :: this

    integer :: i, j
    do i=1, size(this%map,1)
      print '(*(a))', (this%map(i,j), j=1, size(this%map,2))
    end do
  end subroutine warehouse_print


  subroutine move(this)
    class(warehouse_t), intent(inout) :: this

    integer :: idir, ncrates, pos(2)
    integer :: tot_crates, tot_walls, tot_free
    logical :: is_free

    tot_free = count(this%map=='.')
    tot_walls = count(this%map=='#')
    tot_crates = count(this%map=='O')
    if (size(this%map)-1 /= tot_free+tot_walls+tot_crates) then
        print *, size(this%map), tot_free, tot_walls, tot_crates
        error stop 'invalid counts'
    end if

    this%orders_pos = this%orders_pos + 1
    if (this%orders_pos>len_trim(this%orders)) then 
      print *, 'end of orders'
      stop 1
    end if
    idir = scan(ch_orders, this%orders(this%orders_pos:this%orders_pos))
    if (idir==0) error stop 'invalid order'

    ! test if we can move
    ncrates = 0
    pos = this%pos
    is_free = .false.
    do
      pos = pos + dirs(:,idir)
      if (any(pos<1) .or. pos(1)>size(this%map,1) .or. pos(2)>size(this%map,2)) then
        error stop 'out of boundary'
      end if
      select case(this%map(pos(1),pos(2)))
      case('.')
        is_free = .true.
        exit
      case('#')
        exit
      case('O')
        ncrates = ncrates+1
      case default
        error stop 'unexected char in map'
      end select
    end do

    if (is_free) then
      ! remove the robot
      if (this%map(this%pos(1),this%pos(2))/='@') error stop 'lost robot'
      this%map(this%pos(1),this%pos(2)) = '.'
      this%pos = this%pos + dirs(:,idir)
      if (this%map(this%pos(1),this%pos(2))=='#') error stop 'can not overwite wall'
      this%map(this%pos(1),this%pos(2)) = '@'
      ! move after last crate
      pos = this%pos + ncrates*dirs(:,idir)
      ! replace free space by the crate
      if (ncrates>0) then
        if (this%map(pos(1),pos(2))/='.') error stop 'no free space'
        this%map(pos(1),pos(2))='O'
      end if
    else
      continue ! can not move
    end if

    if ( tot_free /= count(this%map=='.') .or.  &
         tot_walls /= count(this%map=='#') .or. &
         tot_crates /= count(this%map=='O') ) &
        error stop 'something wrong after movement'
    if (this%map(this%pos(1),this%pos(2))/='@') &
        error stop 'robot invalid position'
  end subroutine move


  recursive function object_canmove(this, objs, idir) result(can_move)
    class(object_t), intent(in) :: this
    type(object_t), intent(in) :: objs(:)
    integer, intent(in) :: idir
    logical :: can_move

    integer :: k

    select case(this%typ)
    case('#')
      can_move = .false.
    case('O')
      k = check_pos(objs, this%pos+dirs(:,idir))
      if (k==0) then
        can_move = .true.
      else
        can_move = object_canmove(objs(k), objs, idir)
      end if
    case default
      error stop 'could not recognize object type'
    end select
  end function object_canmove


  function check_pos(hay, pos) result(needle)
    class(object_t), intent(in) :: hay(:)
    integer, intent(in) :: pos(2)
    integer :: needle

    integer :: i

    do i=1, size(hay)
      if (all(hay(i)%pos==pos)) exit
    end do
    if (i==size(hay)+1) then
      needle = 0 ! no object at that position
    else
      needle = i
    end if
  end function check_pos

end module day2415_mod