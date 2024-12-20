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

    integer :: s(NSTAT), j, nxy(2)

    ! initialize djikstra
    this%nq = 0
    this%visited = .false.
    this%dis = huge(this%dis)
!this%prev = 0
    this%dis(this%spos(1), this%spos(2)) = 0
    call add_to_queue(this, [this%spos(1), this%spos(2)])

    MAIN_LOOP: do
      if (this%nq==0) exit ! queue is empty
      call top_from_queue(this, s)
 !print *, 'visiting node ',s
      this%visited(s(1),s(2)) = .true.

      do j=1, size(DIRS,2)
        nxy = s(1:2)+DIRS(:,j)
        if (this%map(nxy(1),nxy(2))/='.') cycle

        if (this%visited(nxy(1), nxy(2))) cycle

        if (this%dis(nxy(1),nxy(2)) == huge(this%dis)) then
          ! new node
          call add_to_queue(this, [nxy(1), nxy(2)])
          this%dis(nxy(1),nxy(2)) = this%dis(s(1),s(2))+1
!         this%prev(:,nxy(1),nxy(2)) = s
        else if (this%dis(nxy(1),nxy(2)) > this%dis(s(1),s(2))+1) then
          ! update distance
          this%dis(nxy(1),nxy(2)) = this%dis(s(1),s(2))+1
!         this%prev(:,nxy(1),nxy(2)) = s
        else
          ! do nothing
        end if
      end do

      ! exit found
      if (all(s==this%epos)) then
        print *, 'Exit found'
        exit MAIN_LOOP
      end if
    end do MAIN_LOOP

    ! Verify that the distances are uniques
    do j=0, maxval(this%dis, mask=this%dis<huge(this%dis))
      if (count(this%dis==j)/=1) then
        print *, 'Error for ',j, count(this%dis==j)
      end if
    end do

print *, 'unpossible paths: ', count(this%map=='.' .and. this%dis==huge(this%dis))
print *, 'maximum path: ', maxval(this%dis, mask=this%dis<huge(this%dis))
  end subroutine djikstra


  subroutine explore_cheats(this, ans)
    type(labyrinth_t), intent(in) :: this
    integer, intent(out) :: ans

    integer :: i, j

    ans = 0
    do i=2,size(this%map,1)-1
    do j=2,size(this%map,2)-1
      if (this%map(i,j)/='#') cycle
      ! check vertical cheat
      if (this%map(i-1,j)=='.' .and. this%map(i+1,j)=='.') then
        associate(saved=>abs(this%dis(i-1,j)-this%dis(i+1,j))-2)
          print *, 'cheat saving ', saved
          if (saved >= 100) then
            ans = ans+1
          end if
        end associate
      end if
      ! check horizontal cheat
      if (this%map(i,j-1)=='.' .and. this%map(i,j+1)=='.') then
        associate(saved=>abs(this%dis(i,j-1)-this%dis(i,j+1))-2)
          print *, 'cheat saving ', saved
          if (saved>=100) then
            ans = ans+1
          end if
        end associate
      end if
    end do
    end do
  end subroutine


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
print '("End position:   ",i3,1x,i3)', this%epos
print '("Start position: ",i3,1x,i3)', this%spos
print *, shape(this%map), count(this%map=='.')

    allocate(this%dis(size(this%map,1),size(this%map,2)))
    allocate(this%visited(size(this%map,1),size(this%map,2)))

  end subroutine labyrinth_readmap


  subroutine print_map(this)
    class(labyrinth_t), intent(in) :: this

    integer :: i, j

    do i=1, size(this%map,1)
      write(*,'(*(a))') this%map(i,:)
    end do
  end subroutine print_map


  subroutine day2420(file)
    character(len=*), intent(in) :: file

    type(labyrinth_t) :: lab
    integer :: ans1

    call labyrinth_readmap(file, lab)
    call print_map(lab)
    call djikstra(lab)
    call explore_cheats(lab, ans1)

    print '("Ans 20/1 ",i0,l2)', ans1, ans1==1360
  end subroutine day2420
end module day2420_mod