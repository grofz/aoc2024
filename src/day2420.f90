module day2420_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter, dimension(2,4) :: DIRS = reshape( &
      & [0, 1, 1, 0, 0, -1, -1, 0], [2,4])
  integer, parameter :: NSTAT = 2

  type labyrinth_t
    character(len=1), allocatable :: map(:,:)
    integer, allocatable :: dis(:,:)
    logical, allocatable :: visited(:,:)
    integer, allocatable :: queue(:,:)
    integer :: spos(2), epos(2), nq
  end type

contains

  subroutine djikstra(this)
    type(labyrinth_t), intent(inout) :: this
!
! To each '.' tile assign its distance from the starting tile
!
    integer :: s(NSTAT), j, nxy(2)

    ! initialize djikstra
    this%nq = 0
    this%visited = .false.
    this%dis = huge(this%dis)
    this%dis(this%spos(1), this%spos(2)) = 0
    call add_to_queue(this, [this%spos(1), this%spos(2)])

    MAIN_LOOP: do
      if (this%nq==0) exit ! queue is empty

      call top_from_queue(this, s)
      this%visited(s(1),s(2)) = .true.

      do j=1, size(DIRS,2)
        nxy = s(1:2)+DIRS(:,j)

        ! look at the neighboring unvisited '.' tile
        if (this%map(nxy(1),nxy(2))/='.') cycle
        if (this%visited(nxy(1), nxy(2))) cycle

        associate(newdis=>this%dis(s(1),s(2))+1)
          if (this%dis(nxy(1),nxy(2)) == huge(this%dis)) then
            ! new node
            call add_to_queue(this, [nxy(1), nxy(2)])
            this%dis(nxy(1),nxy(2)) = newdis
          else if (this%dis(nxy(1),nxy(2)) > newdis) then
            ! old node, update distance
            this%dis(nxy(1),nxy(2)) = newdis
          end if
        end associate
      end do

      ! exit found
      if (all(s==this%epos)) exit MAIN_LOOP
    end do MAIN_LOOP

    ! Verify that the distances are uniques
    do j=0, maxval(this%dis, mask=this%dis<huge(this%dis))
      if (count(this%dis==j)/=1) then
        print *, 'Error for ',j, count(this%dis==j)
      end if
    end do

  end subroutine djikstra


  subroutine explore_cheats(this, ch_time, ans)
    type(labyrinth_t), intent(in) :: this
    integer, intent(in) :: ch_time
    integer, intent(out) :: ans

    integer :: i, j, ii, jj

    ans = 0
    ! for all '.' tiles
    do i=2,size(this%map,1)-1
    do j=2,size(this%map,2)-1
      if (this%map(i,j)/='.') cycle
      ! now explore all '.' tiles with Manhattan distance up to ch_time
      do ii = -ch_time, ch_time
      do jj = -ch_time, ch_time
        if (abs(ii)+abs(jj) > ch_time) cycle
        if (ii==0 .and. jj==0) cycle
        if (i+ii < 1 .or. j+jj < 1 .or. i+ii>size(this%map,1) .or. j+jj>size(this%map,2)) cycle
        if (this%map(i+ii,j+jj)/='.') cycle

        associate(saved=>this%dis(i+ii,j+jj)-this%dis(i,j)-abs(ii)-abs(jj))
          !print *, 'cheat saving ', saved
          if (saved >= 100) ans = ans+1
        end associate

      end do
      end do

    end do
    end do
  end subroutine explore_cheats


  subroutine add_to_queue(this, s)
    type(labyrinth_t), intent(inout) :: this
    integer, intent(in) :: s(NSTAT)

    integer :: i
    integer, allocatable :: tmp(:,:)

    ! Make sure there is a space in the queue
    if (.not. allocated(this%queue)) allocate(this%queue(NSTAT,8))
    if (this%nq == size(this%queue,2)) then
      allocate(tmp(NSTAT, this%nq*2))
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
    integer, intent(out) :: top_s(NSTAT)

    integer :: i, k, min_k

    if (this%nq < 1) error stop 'queue is empty'
    min_k = huge(min_k)
    k = 1
    do i=1, this%nq
      associate(s=>this%queue(:,i))
        associate(node=>this%dis(s(1),s(2)))
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


  subroutine labyrinth_readmap(file, this)
    character(len=*), intent(in) :: file
    type(labyrinth_t), intent(out) :: this

    this%map = read_pattern(file)
    if (count(this%map=='E')/=1) error stop 'not just one E'
    this%epos = findloc(this%map,'E')
    if (count(this%map=='S')/=1) error stop 'not just one S'
    this%spos = findloc(this%map,'S')
    this%map(this%spos(1),this%spos(2)) = '.'
    this%map(this%epos(1),this%epos(2)) = '.'

    allocate(this%dis(size(this%map,1),size(this%map,2)))
    allocate(this%visited(size(this%map,1),size(this%map,2)))
  end subroutine labyrinth_readmap


  subroutine print_map(this)
    class(labyrinth_t), intent(in) :: this

    integer :: i

    do i=1, size(this%map,1)
      write(*,'(*(a))') this%map(i,:)
    end do
  end subroutine print_map


  subroutine day2420(file)
    character(len=*), intent(in) :: file

    type(labyrinth_t) :: lab
    integer :: ans1, ans2

    call labyrinth_readmap(file, lab)
!!  call print_map(lab)
    call djikstra(lab)
    call explore_cheats(lab, 2, ans1)
    call explore_cheats(lab, 20, ans2)

    print '("Ans 20/1 ",i0,l2)', ans1, ans1==1360
    print '("Ans 20/2 ",i0,l2)', ans2, ans2==1005476
  end subroutine day2420

end module day2420_mod