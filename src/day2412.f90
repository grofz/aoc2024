module day2412_mod
  use parse_mod, only : read_pattern
  implicit none

  type garden_t
    integer, allocatable :: id(:,:), ngbs(:,:)
    character(len=1), allocatable :: map(:,:)
    integer, allocatable :: area(:), peri(:), side(:)
  end type

  type edgelist_t
    integer, allocatable :: a(:,:)
    integer :: n=0
  end type
  integer, parameter :: EDGE_COMPONENTS = 5

contains

  subroutine day2412(file)
    character(len=*), intent(in) :: file

    type(garden_t) :: garden
    integer :: ans1, ans2

    garden%map = read_pattern(file)
    call label_connected(garden)
    call count_fences(garden)
    ans1 = sum(garden%area * garden%peri)
    ans2 = sum(garden%area * garden%side)
    print '("Ans 12/1 ",i0,l2)', ans1, ans1==1533024
    print '("Ans 12/2 ",i0,l2)', ans2, ans2==910066

  end subroutine day2412


  subroutine label_connected(this)
    type(garden_t), intent(inout) :: this

    integer :: i, j, id

    if (allocated(this%id)) deallocate(this%id)
    allocate(this%id(size(this%map,1), size(this%map,2)), source=0)

    id = 0
    do i=1, size(this%id,1)
    do j=1, size(this%id,2)
      if (this%id(i,j)/=0) cycle
      id = id+1
      call crawl(i, j, this, id, this%map(i,j))
    end do
    end do
print '("Labeled components ",i0)', id

    if (allocated(this%area)) deallocate(this%area)
    allocate(this%area(id))
    do i=1, id
      this%area(i) = count(this%id==i)
    end do
  end subroutine label_connected


  subroutine count_fences(this)
    type(garden_t), intent(inout) :: this

    integer :: i, j, di, dj, dj2, di2
    type(edgelist_t) :: edges
    integer :: edge(EDGE_COMPONENTS)


    ! Helper array "ngbs" - to store the number of alien neighbours
    if (allocated(this%ngbs)) deallocate(this%ngbs)
    allocate(this%ngbs(size(this%map,1),size(this%map,2)), source=4)
    do i=1,size(this%ngbs,1)
    do j=1,size(this%ngbs,2)
      do di=-1,1
      do dj=-1,1
        if (abs(di)+abs(dj)/=1) cycle

        ! what the edge would look like?
        dj2 = (dj-1)/2
        di2 = (di-1)/2
        if (di==0) then
          if (dj>0) then
            ! ngb at right - vector heading down
            edge = [i-1, j+dj2, i, j+dj2, this%id(i,j)]
          else
            ! ngb at left - vector heading up
            edge = [i, j+dj2, i-1, j+dj2, this%id(i,j)]
          end if
        else
          if (di>0) then
            ! ngb is below - vector heading left
            edge = [i+di2, j, i+di2, j-1, this%id(i,j)]
          else
            ! ngb is above - vector heading right
            edge = [i+di2, j-1, i+di2, j, this%id(i,j)]
          end if
        end if

        ! for each of four neighbours
        if (i+di<1 .or. j+dj<1 .or. i+di>size(this%ngbs,1) .or. &
            &                       j+dj>size(this%ngbs,2)) then
          ! add edge if at the border of the map
          call add_edge(edges, edge)
        else if (this%map(i+di,j+dj)/=this%map(i,j)) then
          ! add edge if neighbour is alien
          call add_edge(edges, edge)
        else
          ! reduce the number of fences if neighbour has the same id
          this%ngbs(i,j) = this%ngbs(i,j)-1
        end if
      end do
      end do
    end do
    end do

    if (allocated(this%peri)) deallocate(this%peri)
    if (allocated(this%side)) deallocate(this%side)
    allocate(this%peri(maxval(this%id)))
    allocate(this%side(maxval(this%id)))

    do
      i = edges%n
      call reduce_edges(edges)
      if (edges%n==i) exit
      print '("Number of edges reduced from ",i0," to ",i0)',i,edges%n
    end do

    do i=1, size(this%peri)
      this%peri(i) = sum(this%ngbs, mask=this%id==i)
      this%side(i) = count(edges%a(5,1:edges%n)==i)
    end do
  end subroutine count_fences


  subroutine reduce_edges(this)
    type(edgelist_t), intent(inout) :: this

    integer :: i, j

    MAIN_LOOP: do i=1, this%n
      do j=1, this%n

        ! ignore removed edges
        if (this%a(5,i) == 0 .or. this%a(5,j) == 0) cycle

        ! proceed only with edges belongig to the same component id
        if (this%a(5,i) /= this%a(5,j)) cycle

        ! both edges must be different
        if (i==j) cycle

 ! TODO us dot product to test co-linearity
        ! both edges must be verrtical/horizontal
        if (.not. is_edge_vertical(this%a(:,i))) then
          if (is_edge_vertical(this%a(:,j))) cycle
        else
          if (.not. is_edge_vertical(this%a(:,j))) cycle
        end if

        ! now the edge should belong to the same id and same orientation
        if (all(this%a(3:4,i)==this%a(1:2,j))) then
          ! end point of "i" is same as beg point of "j"
          ! combine edges into "i" and remove "j"
          this%a(3:4,i)=this%a(3:4,j)
          this%a(:,j) = 0
          !exit MAIN_LOOP
        end if
      end do
    end do MAIN_LOOP

    ! compact edge list by filling the gaps
    i = 1
    do
      if (i>this%n) exit
      if (all(this%a(:,i)==0)) then
        this%a(:,i) = this%a(:,this%n)
        this%n = this%n - 1
      else
        i = i + 1
      end if
    end do
  end subroutine reduce_edges


  function is_edge_vertical(edge) result(yes)
    integer, intent(in), dimension(EDGE_COMPONENTS) :: edge(:)
    logical :: yes

    associate(di => edge(3)-edge(1), dj => edge(4)-edge(2))
      if ((di/=0 .and. dj/=0) .or. (di==0 .and. dj==0)) &
          & error stop 'only vertical/horizontal edges are allowed'
      yes = dj==0
    end associate
  end function


  recursive subroutine crawl(i, j, this, id, ch)
    integer, intent(in) :: i, j, id
    character(len=*), intent(in) :: ch
    type(garden_t), intent(inout) :: this

    integer :: di, dj

    ! verify on a correct square
    if (i<1 .or. j<1 .or. i>size(this%map,1) .or. j>size(this%map,2)) return
    if (this%map(i,j)/=ch) return
    if (this%id(i,j)==id) return

    ! mark the current node by selected id and crawl over all its neigbours
    this%id(i,j) = id
    do di=-1,1
    do dj=-1,1
      if (abs(di)+abs(dj)/=1) cycle
      call crawl(i+di, j+dj, this, id, ch)
    end do
    end do
  end subroutine crawl


  subroutine add_edge(this,edge)
    class(edgelist_t), intent(inout) :: this
    integer, intent(in) :: edge(:)

    integer, allocatable :: tmp(:,:)

    ! make sure there is a place for the edge
    if (.not. allocated(this%a)) then
      allocate(this%a(EDGE_COMPONENTS,8))
      this%n = 0
    end if
    if (size(this%a,2)==this%n) then
      allocate(tmp(EDGE_COMPONENTS,this%n*2))
      tmp(:,1:this%n) = this%a
      call move_alloc(tmp, this%a)
    end if

    ! add the edge
    this%n = this%n + 1
    this%a(:,this%n) = edge
  end subroutine add_edge


end module day2412_mod
