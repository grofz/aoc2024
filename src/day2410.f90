module day2410_mod
  use parse_mod, only : read_pattern
  implicit none

  ! dynamic array to store [i,j] of several points on the map
  type list_t
    integer :: n=0
    integer, allocatable :: pos(:,:)
  contains
    procedure :: add => add_to_list
    procedure :: clear => clear_list
  end type

contains

  subroutine day2410(file)
    character(len=*), intent(in) :: file
    
    integer, allocatable :: map(:,:)
    integer :: i, j, ans1, ans2
    type(list_t) :: tails

    map = read_map_from_file(file)
    ans1 = 0
    ans2 = 0
    do i=1,size(map,1)
    do j=1,size(map,2)
      ! Loop over all trail_heads
      if (map(i,j)/=0) cycle

      ! Part 1 - the number of distinct trail_tails
      call trail_tail([i,j], map, tails, .false.)
      ans1 = ans1 + tails%n
      call tails%clear()

      ! Part 2 - the number of distinct paths
      call trail_tail([i,j], map, tails, .true.)
      ans2 = ans2 + tails%n
      call tails%clear()
    end do
    end do

    print '("Ans 10/1 ",i0,l2)', ans1, ans1==816
    print '("Ans 10/2 ",i0,l2)', ans2, ans2==1960
  end subroutine day2410


  recursive subroutine trail_tail(pos, map, tails, is_part2)
    integer, intent(in) :: pos(:), map(:,:)
    type(list_t), intent(out) :: tails
    logical, intent(in) :: is_part2

    type(list_t) :: tails0, paths
    integer :: i, j

    if (map(pos(1),pos(2))==9) then
      ! end of trail has been reached, returns a single item in the list
      call tails%add(pos)
      return
    end if

    paths = next_paths(pos, map)
    do i=1,paths%n
      call trail_tail(paths%pos(:,i), map, tails0, is_part2)

      ! combine results from all sub-paths
      do j=1, tails0%n
        call tails%add(tails0%pos(:,j), is_part2)
      end do
      call tails0%clear()
    end do
  end subroutine trail_tail


  function next_paths(pos, map) result(nexts)
    integer, intent(in) :: pos(2), map(:,:)
    type(list_t) :: nexts

    integer :: i, j, di, dj

    do di=-1,1
    do dj=-1,1
      if (abs(di)+abs(dj) /= 1) cycle
      i = pos(1) + di
      j = pos(2) + dj
      if (i<1 .or. j<1 .or. i>size(map,1) .or. j>size(map,2)) cycle
      if (map(i,j) /= map(pos(1),pos(2))+1) cycle
      ! found a possible continuation of the path
      call nexts%add([i,j])
    end do
    end do
  end function next_paths


  function read_map_from_file(file) result(map)
    character(len=*), intent(in) :: file
    integer, allocatable :: map(:,:)

    character(len=1), allocatable :: map_ch(:,:)
    integer :: i, j

    map_ch = read_pattern(file)
    allocate(map(size(map_ch,1), size(map_ch,2)))
    do i=1,size(map,1)
    do j=1,size(map,2)
      read(map_ch(i,j),*) map(i,j)
    end do
    end do
  end function read_map_from_file


  subroutine add_to_list(this, pos, allow_dups)
    class(list_t), intent(inout) :: this
    integer, intent(in) :: pos(2)
    logical, intent(in), optional :: allow_dups
!
! Add "pos" to the list. Duplicate items are not allowed on default
!
    integer, allocatable :: wrk(:,:)
    integer :: i
    logical :: allow_dups0

    allow_dups0 = .false.
    if (present(allow_dups)) allow_dups0 = allow_dups

    ! initialize list if necessary
    if (.not. allocated(this%pos)) then
      allocate(this%pos(2,4))
      this%n=0
    end if

    ! verify the position is not already in the list
    if (.not. allow_dups0) then
      do i=1, this%n
        if (all(pos==this%pos(:,i))) return
      end do
    end if

    ! extend the dynamic array if necessary
    if (size(this%pos,2)==this%n) then
      allocate(wrk(2,this%n*2))
      wrk(:,1:this%n) = this%pos
      call move_alloc(wrk, this%pos)
    end if

    ! finaly, add the position to the list
    this%n = this%n + 1
    this%pos(:,this%n) = pos
  end subroutine add_to_list


  subroutine clear_list(this)
    class(list_t), intent(inout) :: this

    if (allocated(this%pos)) deallocate(this%pos)
    this%n = 0
  end subroutine clear_list

end module day2410_mod