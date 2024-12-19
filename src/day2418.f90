module day2418_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  integer, parameter :: LABYRINTH_SIZE = 71

  integer, parameter :: NSTATES = 3 ! x,y,time

  type labyrinth_t
    ! when is map tile blocked by folling rock
    integer, dimension(LABYRINTH_SIZE, LABYRINTH_SIZE) :: blox, dist
    logical, dimension(LABYRINTH_SIZE, LABYRINTH_SIZE) :: visited, path
    integer, dimension(NSTATES, LABYRINTH_SIZE, LABYRINTH_SIZE) :: prev
    integer, allocatable :: queue(:,:)
    integer :: nq=0
  end type

  integer, parameter, dimension(2,4) :: DIRS = reshape( &
      & [ 1, 0, 0, 1, -1, 0, 0, -1], [2,4])

contains

  subroutine djikstra(this, time, lim_time)
    type(labyrinth_t), intent(inout) :: this
    integer, intent(in) :: time
    integer, intent(out) :: lim_time

    integer :: s(NSTATES), j, nxy(2)

    ! initialize djikstra
    this%nq = 0
    this%visited = .false.
    this%dist = huge(this%dist)
    this%prev = 0
    associate(start_x=>1, start_y=>1)
      this%dist(start_x, start_y) = 0
      call add_to_queue(this, [start_x, start_y, 0])
    end associate

    MAIN_LOOP: do
      if (this%nq==0) exit ! queue is empty
      call top_from_queue(this, s)
!print *, 'visiting node ',s
      this%visited(s(1),s(2)) = .true.

      do j=1, size(DIRS,2)
        nxy = next_pos(s(1:2), j)
        if (.not. safe_pos(nxy, this%blox, time)) cycle
!       if (.not. safe_pos(nxy, this%blox, 12)) cycle

        if (this%visited(nxy(1), nxy(2))) cycle

        if (this%dist(nxy(1),nxy(2)) == huge(this%dist)) then
          ! new node
          call add_to_queue(this, &
          & [ nxy(1), nxy(2), this%dist(s(1),s(2))+1 ])
          this%dist(nxy(1),nxy(2)) = this%dist(s(1),s(2))+1
          this%prev(:,nxy(1),nxy(2)) = s
        else if (this%dist(nxy(1),nxy(2)) > this%dist(s(1),s(2))+1) then
          ! update distance
          this%dist(nxy(1),nxy(2)) = this%dist(s(1),s(2))+1
          this%prev(:,nxy(1),nxy(2)) = s
        else
          ! do nothing
        end if
      end do

      if (s(1)==LABYRINTH_SIZE .and. s(2)==LABYRINTH_SIZE) then
        ! exit found
        exit MAIN_LOOP
      end if

    end do MAIN_LOOP

    ! back-track
    this%path = .false.
    s = [LABYRINTH_SIZE, LABYRINTH_SIZE, 1]
    do
      if (any(s==0)) exit
      this%path(s(1),s(2)) = .true.
      s = this%prev(:,s(1),s(2))
    end do
    lim_time = minval(this%blox, mask=this%path)
  end subroutine djikstra


  subroutine read_labyrinth_blox(file, blox)
    character(len=*), intent(in) :: file
    integer, intent(out) :: blox(:,:)

    type(string_t), allocatable :: lines(:), tokens(:)
    integer :: i, j, k

    lines = read_strings(file)

    blox = huge(blox) ! tiles are free on default
    do i=1,size(lines)
      call split(lines(i)%str, ',', tokens)
      read(tokens(1)%str,*) j
      read(tokens(2)%str,*) k
      blox(j+1,k+1) = i ! zero-based to one-based co-rdinate conversion
    end do
  end subroutine read_labyrinth_blox


  pure function next_pos(xy, d) result(nxy)
    integer, intent(in) :: xy(2), d
    integer :: nxy(2)
    nxy = xy + DIRS(:,d)
  end function next_pos


  pure function safe_pos(xy, blox, time) result(safe)
    integer, intent(in) :: xy(2), time, blox(:,:)
    logical :: safe

    if (xy(1)<1 .or. xy(2)<1 .or. xy(1)>LABYRINTH_SIZE .or. xy(2)>LABYRINTH_SIZE) then
      safe = .false.
    else if (blox(xy(1),xy(2)) <= time) then
      safe = .false.
    else
      safe = .true.
    end if
  end function safe_pos


  subroutine print_map(blox, time)
    integer, intent(in) :: blox(:,:), time

    integer :: x, y
    character(len=1) :: ch

    write(*,*)
    do y=1, LABYRINTH_SIZE
      do x=1, LABYRINTH_SIZE
        if (blox(x,y) <= time) then
          ch = '#'
        else
          ch = '.'
        end if
        write(*,'(a1)',advance='no') ch
      end do
      write(*,*)
    end do
  end subroutine print_map


  subroutine day2418(file)
    character(len=*), intent(in) :: file

    type(labyrinth_t) :: lab
    integer :: ans1, i, xy(2), lim_time
    character(len=20) :: ans2

    call read_labyrinth_blox(file, lab%blox)
!!  call print_map(lab%blox, 1024)
    call djikstra(lab, 1024, lim_time)
    ans1 = lab%dist(LABYRINTH_SIZE, LABYRINTH_SIZE)

    i = 1024
    do
      call djikstra(lab, i, lim_time)
!!print '("time: ",i0,3x, "shortest path: ",i0)', i, lab%dist(LABYRINTH_SIZE,LABYRINTH_SIZE)
      if (lab%dist(LABYRINTH_SIZE,LABYRINTH_SIZE) /= huge(lab%dist)) then
        i = lim_time
        cycle
      end if
      exit
    end do
    xy = findloc(lab%blox, i)-1
    write(ans2,'(i0,",",i0)') xy

    print '("Ans 18/1 ", i0, l2)', ans1, ans1==226
    print '("Ans 18/1 ", a, l2)', trim(ans2), ans2=='60,46'
  end subroutine day2418


  subroutine add_to_queue(this, s)
    type(labyrinth_t), intent(inout) :: this
    integer, intent(in) :: s(NSTATES)

    integer :: i
    integer, allocatable :: tmp(:,:)

    ! Make sure there is a space in the queue
    if (.not. allocated(this%queue)) allocate(this%queue(NSTATES,8))
    if (this%nq == size(this%queue,2)) then
      allocate(tmp(NSTATES, this%nq*2))
      tmp(:, 1:this%nq) = this%queue
      call move_alloc(tmp, this%queue)
    end if

    ! Make sure node is not yet in the queue
    do i=1, this%nq
      if (all(this%queue(:,i)==s)) then
print *, 'already in queue'
        error stop
      end if
    end do

    ! add at the end of queue
    this%nq = this%nq+1
    this%queue(:,this%nq) = s
  end subroutine add_to_queue


  subroutine top_from_queue(this, top_s)
    class(labyrinth_t), intent(inout) :: this
    integer, intent(out) :: top_s(NSTATES)

    integer :: i, k, min_k

    if (this%nq < 1) error stop 'queue is empty'
    min_k = huge(min_k)
    k = 1
    do i=1, this%nq
      associate(s=>this%queue(:,i))
        associate(node=>this%dist(s(1),s(2)))
          if (node < min_k) then
            k = i
            min_k = node
          end if
        end associate
      end associate
    end do
    top_s = this%queue(:,k)

    ! remove from queue
    this%queue(:,k) = this%queue(:,this%nq)
    this%nq = this%nq - 1
  end subroutine top_from_queue

end module day2418_mod