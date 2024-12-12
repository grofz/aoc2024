module day2412_mod
  use parse_mod, only : read_pattern
  use iso_fortran_env, only : dp => real64
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
  integer, parameter :: EDGE_COMPONENTS = 4

contains

  subroutine day2412(file)
    character(len=*), intent(in) :: file

    type(garden_t) :: garden
    integer :: ans1, ans2
    real(dp) :: time0, time1

    call cpu_time(time0)
    garden%map = read_pattern(file)
    call label_connected(garden)
    call count_fences(garden)
    ans1 = sum(garden%area * garden%peri)
    ans2 = sum(garden%area * garden%side)
    call cpu_time(time1)
    print '("Ans 12/1 ",i0,l2)', ans1, ans1==1533024
    print '("Ans 12/2 ",i0,l2)', ans2, ans2==910066
    print '("  elapsed time ",f6.3," s")', time1-time0

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

    if (allocated(this%area)) deallocate(this%area)
    allocate(this%area(id))
    do i=1, id
      this%area(i) = count(this%id==i)
    end do
  end subroutine label_connected


  subroutine count_fences(this)
    type(garden_t), intent(inout) :: this

    integer :: i, j, di, dj, dj2, di2, k, ncomp
    type(edgelist_t), allocatable :: edges(:)
    integer :: edge(EDGE_COMPONENTS)

    integer, parameter, dimension(*,*) :: DIR=reshape( &
      [0, -1, -1, 0, 0, 1, 1, 0], [2,4])
    integer, parameter :: ID_LEFT=1, ID_DOWN=2, ID_RIGHT=3, ID_UP=4
    logical :: ngb_alien

    ncomp = maxval(this%id)

    ! Perimeter of every region and helper array "ngbs"
    ! - to store the number of alien neighbours
    if (allocated(this%peri)) deallocate(this%peri)
    allocate(this%peri(ncomp))
    if (allocated(this%ngbs)) deallocate(this%ngbs)
    allocate(this%ngbs(size(this%map,1),size(this%map,2)), source=4)

    ! Number of sides for every region and helper array of edgelists
    if (allocated(this%side)) deallocate(this%side)
    allocate(this%side(ncomp))
    allocate(edges(ncomp))

    ! Loop over the map
    do i=1,size(this%ngbs,1)
    do j=1,size(this%ngbs,2)
      NGB_LOOP: do k=1,4

        di = DIR(1,k)
        dj = DIR(2,k)
        dj2 = (dj-1)/2
        di2 = (di-1)/2
        if (i+di<1 .or. j+dj<1 .or. i+di>size(this%ngbs,1) .or. &
            &                       j+dj>size(this%ngbs,2)) then
          ngb_alien = .true.
        else
          ngb_alien = this%map(i+di,j+dj)/=this%map(i,j)
        end if

        if (.not. ngb_alien) then
          this%ngbs(i,j) = this%ngbs(i,j)-1
        else
          select case(k)
          case(ID_LEFT)
            edge = [i, j+dj2, i-1, j+dj2]
          case(ID_RIGHT)
            edge = [i-1, j+dj2, i, j+dj2]
          case(ID_DOWN)
            edge = [i+di2, j, i+di2, j-1]
          case(ID_UP)
            edge = [i+di2, j-1, i+di2, j]
          end select
          call add_edge(edges(this%id(i,j)), edge)
        end if

      end do NGB_LOOP
    end do
    end do

    do k=1, ncomp
      this%peri(k) = sum(this%ngbs, mask=this%id==k)
      do
        i = edges(k)%n
        call reduce_edges(edges(k))
        if (edges(k)%n==i) exit
!!      print '("Number of edges reduced from ",i0," to ",i0)',i,edges(k)%n
      end do
      this%side(k) = edges(k)%n
    end do
  end subroutine count_fences


  subroutine reduce_edges(this)
    type(edgelist_t), intent(inout) :: this

    integer :: i, j
    logical, allocatable :: active(:)

    allocate(active(this%n), source= .true.)
    MAIN_LOOP: do i=1, this%n
      do j=1, this%n

        ! ignore removed edges
        if (.not. (active(i) .and. active(j))) cycle

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
!         this%a(:,j) = 0
          active(j) = .false.
        end if
      end do
    end do MAIN_LOOP

    ! compact edge list by filling the gaps
    i = 1
    do
      if (i>this%n) exit
      if (.not. active(i)) then
        this%a(:,i) = this%a(:,this%n)
        active(i) = active(this%n)
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
