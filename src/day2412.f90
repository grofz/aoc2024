module day2412_mod
  use parse_mod, only : read_pattern
  implicit none

  type garden_t
    integer, allocatable :: id(:,:), ngbs(:,:)
    character(len=1), allocatable :: map(:,:)
    integer, allocatable :: area(:), peri(:), sides(:)
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
    ans1 = sum(garden%area*garden%peri)
    ans2 = sum(garden%area*garden%sides)

 print *, 'Areas =', garden%area
 print *, 'Perim =', garden%peri
 print *, 'Sides =', garden%sides
!print *, sum(this%area), size(this%map)

    print '("Ans 12/1 ",i0,l2)', ans1, ans1==1533024
    print '("Ans 12/2 ",i0,l2)', ans2, ans2==1         ! 903238 too low 821255

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
print *, 'Labeled components = ', id

    if (allocated(this%area)) deallocate(this%area)
    allocate(this%area(id))
    do i=1, id
      this%area(i) = count(this%id==i)
    end do
  end subroutine label_connected


  subroutine count_fences(this)
    type(garden_t), intent(inout) :: this

    integer :: i, j, di, dj
    type(edgelist_t) :: edges
    integer :: edge(EDGE_COMPONENTS)


    if (allocated(this%ngbs)) deallocate(this%ngbs)
    allocate(this%ngbs(size(this%map,1),size(this%map,2)), source=4)
    do i=1,size(this%ngbs,1)
    do j=1,size(this%ngbs,2)
      do di=-1,1
      do dj=-1,1
        if (abs(di)+abs(dj)/=1) cycle

        ! what the edge would look like?
        if (di==0) then
          ! horizontal
          if (dj>0) then
           !edge = [i, j+dj, i-1, j+dj, this%id(i,j)]
            edge = [i-1, j+dj, i, j+dj, this%id(i,j)]
          else
            edge = [i-1, j+dj, i, j+dj, this%id(i,j)]
          end if
        else
          ! vertical
          if (di>0) then
            edge = [i+di, j-1, i+di, j, this%id(i,j)]
          else
           !edge = [i+di, j, i+di, j-1, this%id(i,j)]
            edge = [i+di, j-1, i+di, j, this%id(i,j)]
          end if
        end if

        ! for each of four neighbours
        if (i+di<1 .or. j+dj<1 .or. i+di>size(this%ngbs,1) .or. &
            &                       j+dj>size(this%ngbs,2)) then
          ! add edge
          call add_edge(edges, edge)
          cycle
        end if
        if (this%map(i+di,j+dj)==this%map(i,j)) then
          this%ngbs(i,j) = this%ngbs(i,j)-1
        else
          call add_edge(edges, edge)
        end if
      end do
      end do
    end do
    end do

    do
      i = edges%n
print *, 'reducing edges ',i
      call reduce_edges(edges)
      if (edges%n==i) exit
    end do


    if (allocated(this%peri)) deallocate(this%peri)
    if (allocated(this%sides)) deallocate(this%sides)
    allocate(this%peri(maxval(this%id)))
    allocate(this%sides(maxval(this%id)))
    do i=1, size(this%peri)
       this%peri(i) = sum(this%ngbs, mask=this%id==i)
!     this%peri(i) = count(edges%a(5,1:edges%n)==i)
      this%sides(i) = count(edges%a(5,1:edges%n)==i)
      block
        integer :: mi, mj
        character(len=1) :: ch
!       write(*,*) 'id =',i
        do mi=1,size(this%map,1)
          do mj=1,size(this%map,2)
            if (this%id(mi,mj)==i) then
              write(ch,'(i1)') this%ngbs(mi,mj)
            else
              ch='.'
            end if
!           write(*,'(a1,1x)',advance='no') ch
          end do
!         write(*,*)
        end do
      end block
    end do
print *, 'edges in total ', edges%n
  end subroutine count_fences


  subroutine reduce_edges(this)
    type(edgelist_t), intent(inout) :: this

    integer :: i, j
print *, 'start = ', this%n

   !MAIN_LOOP: do i=1, this%n
    MAIN_LOOP: do i=this%n,1 ,-1
      do j=this%n,1, -1
     !do j=1, this%n
        if (i==j) cycle

        if (this%a(5,i) /= this%a(5,j)) cycle
        if (this%a(5,i) == 0) cycle
        if (this%a(5,j) == 0) cycle

        if (this%a(1,i)==this%a(3,i)) then
          ! "i" is horizontal
          if (this%a(1,j)==this%a(3,j)) then
            continue
          elseif (this%a(2,j)==this%a(4,j)) then
            cycle
          else
            error stop 'can not be'
          end if

        elseif (this%a(2,i)==this%a(4,i)) then
          ! "i" is vertical
          if (this%a(1,j)==this%a(3,j)) then
            cycle
          elseif (this%a(2,j)==this%a(4,j)) then
            continue
          else
            error stop 'can not be'
          end if
        else
          error stop 'can not be'
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

    ! compact edge list
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
print *, 'end = ', this%n
  end subroutine reduce_edges


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