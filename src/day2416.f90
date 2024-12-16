module day2416_mod
  use parse_mod, only : read_pattern
  implicit none

  ! east, south, west, north
  integer, parameter, dimension(2,4) :: HEADINGS = reshape( &
    & [0, 1, 1, 0, 0, -1, -1, 0], [2,4])
  ! cost to change direction(old_heading, new_heading)
  integer, parameter, dimension(4,4) :: TURN_MAT = reshape( &
    & [0, 1, 2, 1,   1, 0, 1, 2,   2, 1, 0, 1,   1, 2, 1, 0], [4,4])

  integer, parameter :: MAX_EDGES = 4, COST_TURN = 1000, COST_MOVE = 1 

  type edge_t
    integer :: beg(3), fin(3), cost
  end type edge_t

  integer, parameter :: mold_int = huge(mold_int)

  type node_t
    type(edge_t) :: elist(MAX_EDGES)
    character(len=1) :: typ
    integer :: ne=0
    logical :: visited = .false.
    integer :: val = huge(mold_int)
  end type

  type labyrinth_t
    character(len=1), allocatable :: map(:,:)
    integer :: start(3), finish(3) ! [row, col, hdg]
    type(node_t), allocatable :: nods(:,:,:)
    integer, allocatable :: queue(:,:)
    integer :: nq=0
  end type labyrinth_t

contains

  subroutine day2416(file)
    character(len=*), intent(in) :: file

    type(labyrinth_t) :: lab
    integer :: ans1

    lab = labyrinth_from_file(file)
    call djikstra(lab)
    ans1 = minval(lab%nods(lab%finish(1),lab%finish(2),:)%val)

    print '("Ans 16/1 ",i0,l2)', ans1, ans1==66404
  end subroutine day2416


  function labyrinth_from_file(file) result(new)
    character(len=*), intent(in) :: file
    type(labyrinth_t) :: new

    new%map = read_pattern(file)
    if (count(new%map=='S')/=1 .or. count(new%map=='E')/=1) &
        error stop 'The map does not contain the starting/ending position'
    new%start(1:2) = findloc(new%map, 'S')
    new%start(3) = 1 ! east
    new%finish(1:2) = findloc(new%map, 'E')
    new%map(new%start(1),new%start(2)) = '.'
    new%map(new%finish(1),new%finish(2)) = '.'

    ! check that only '.' and '#' are in the map
    block
      integer :: n1, n2
      n1 = count(new%map=='#')
      n2 = count(new%map=='.')
      print '("There are ",i0," walls and ",i0," spaces. Check is ",l2)', &
          n1, n2, n1+n2 == size(new%map)
      print '("Start:  ",3(i0,1x))', new%start
      print '("Finish: ",3(i0,1x))', new%finish
    end block

    ! start building the graph
    allocate(new%nods(size(new%map,1), size(new%map,2), 4))
    where(new%map=='.')
      new%nods(:,:,1)%typ = '.'
      new%nods(:,:,2)%typ = '.'
      new%nods(:,:,3)%typ = '.'
      new%nods(:,:,4)%typ = '.'
    else where
      new%nods(:,:,1)%typ = '#'
      new%nods(:,:,2)%typ = '#'
      new%nods(:,:,3)%typ = '#'
      new%nods(:,:,4)%typ = '#'
    end where

    ! add edges connecting the nearest neighbours
    block
      integer :: i, j, k, m, ngbi, ngbj, cost
      do i=1, size(new%nods,1)
      do j=1, size(new%nods,2)
        if (new%nods(i,j,1)%typ=='#') cycle
        do k=1, size(new%nods,3)
          do m=1, 4
            ngbi = i + HEADINGS(1, m)
            ngbj = j + HEADINGS(2, m)
            cost = COST_TURN*TURN_MAT(k,m) + COST_MOVE
            if (new%nods(ngbi, ngbj, 1)%typ == '#') cycle
            call addedge([i,j,k], new%nods, [ngbi, ngbj, m], cost)
          end do
        end do
      end do
      end do
    end block
print *, 'total edges ', sum(new%nods%ne)
print *, 'total nodes ', count(new%nods%ne>0)
  end function labyrinth_from_file


  subroutine djikstra(this)
    type(labyrinth_t), intent(inout) :: this

    integer :: cur_s(3), i


    ! prepare algorithm
    this%nods%val = huge(this%nods%val)
    this%nods%visited = .false.
    associate(s=>this%start)
      this%nods(s(1),s(2),s(3))%val = 0
      call add_to_queue(this, s)
    end associate

    ! main loop
    MAIN_LOOP: do
      call top_from_queue(this, cur_s)

      ! visit all unvisited neighbours of cur_s
      associate(cn => this%nods(cur_s(1),cur_s(2),cur_s(3)))
        NGB_LOOP: do i=1, cn%ne
          associate(e=>cn%elist(i))
            if (any(e%beg /= cur_s)) error stop 'wrong edge'
            associate(en => this%nods(e%fin(1),e%fin(2),e%fin(3)))
              if (en%visited) cycle
              if (en%val==huge(mold_int)) then
                ! new node found, add it to queue
                en%val = cn%val + e%cost
                call add_to_queue(this, e%fin)
              else if (en%val > cn%val + e%cost) then
                ! shorter route found, must still be in queue
                en%val = cn%val + e%cost
              else
                ! found longer route, do nothing
                continue
              end if
            end associate
          end associate
        end do NGB_LOOP

        ! mark node as visited
        cn%visited = .true.

        ! check if it is end
        if (cur_s(1)==this%finish(1) .and. cur_s(2)==this%finish(2)) then
print *, 'something found =', cur_s, cn%val
        end if
      end associate

      if (this%nq==0) exit MAIN_LOOP
    end do MAIN_LOOP
  end subroutine djikstra


  subroutine addedge(s, nods, f, c)
    integer, intent(in) :: s(3), f(3), c
    type(node_t), intent(inout) :: nods(:,:,:)

    ! see, if edge is there
    ! TODO

    ! see, if space remains
    associate(this=>nods(s(1),s(2),s(3)))
      if (this%ne==MAX_EDGES) error stop 'add edge - can not add more edges'
      this%ne = this%ne + 1
      this%elist(this%ne)%beg = s
      this%elist(this%ne)%fin = f
      this%elist(this%ne)%cost = c
    end associate
  end subroutine addedge


  subroutine add_to_queue(this, s)
    type(labyrinth_t), intent(inout) :: this
    integer, intent(in) :: s(3)

    integer :: i

    ! Make sure there is a space in the queue
    if (.not. allocated(this%queue)) allocate(this%queue(3,8))
    if (this%nq == size(this%queue,2)) then
      block
        integer, allocatable :: tmp(:,:)
        allocate(tmp(3,this%nq*2))
        tmp(:,1:this%nq) = this%queue
        call move_alloc(tmp, this%queue)
      end block
    end if

    ! Make sure node is not yet in the queue
    do i=1, this%nq
      if (all(this%queue(:,i)==s)) then
print *, 'already in queue'
        error stop
        exit
      end if
    end do

    ! add at the end of queue
    this%nq = this%nq+1
    this%queue(:,this%nq) = s
  end subroutine add_to_queue


  subroutine top_from_queue(this, top_s)
    class(labyrinth_t), intent(inout) :: this
    integer, intent(out) :: top_s(3)

    integer :: i, k, min_k

    if (this%nq < 1) error stop 'queue is empty'
    min_k = huge(min_k)
    k = 1
    do i=1, this%nq
      associate(s=>this%queue(:,i))
        associate(node=>this%nods(s(1),s(2),s(3)))
          if (node%val < min_k) then
            k = i
            min_k = node%val
          end if
        end associate
      end associate
    end do
    top_s = this%queue(:,k)
    ! remove from queue
    this%queue(:,k) = this%queue(:,this%nq)
    this%nq = this%nq - 1
  end subroutine top_from_queue



end module day2416_mod