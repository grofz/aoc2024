module day2406_mod
  use parse_mod, only : read_pattern
  implicit none

  type agent_t
    integer :: pi, pj ! the position in the map
    integer :: dr     ! the heading (1=north,2=east,3=south,4=west)
    logical, allocatable :: timespace(:,:,:)
      ! were we at this position at this heading already?
  contains
    procedure :: onestep, put_at_start, measure_path
  end type agent_t

  integer, parameter, dimension(2,4):: dirs = reshape([ &
    -1, 0, & ! north
     0, 1, & ! east
     1, 0, & ! south
     0,-1  & ! west
     ], [2,4])

  integer, parameter :: IN_LOOP = -1

contains

  subroutine measure_path(this, map, pathlen)
    class(agent_t), intent(inout) :: this
    character(len=1), intent(in) :: map(:,:)
    integer, intent(out) :: pathlen

    logical :: dejavu

    call this%put_at_start(map)
    do while (this%pi/=0 .and. this%pj/=0)
      call this%onestep(map, dejavu)
      if (dejavu) then
        pathlen = IN_LOOP
        return
      end if
    end do
    pathlen = count(any(this%timespace,dim=3))
  end subroutine measure_path


  subroutine put_at_start(this, map)
    class(agent_t), intent(inout) :: this
    character(len=1), intent(in) :: map(:,:)
!
! Put the agent at the position "^" in the map.
!
    integer :: pij(2)

    pij = findloc(map,'^')
    this%pi = pij(1)
    this%pj = pij(2)
    this%dr = 1

    ! make sure size of "timespace" is consistent with size of "map"
    ! initialize "timespace"
    if (allocated(this%timespace)) then
      if (size(this%timespace,1)/=size(map,1) .or. &
          size(this%timespace,2)/=size(map,2)) deallocate(this%timespace)
    end if
    if (.not. allocated(this%timespace)) then
      allocate(this%timespace(size(map,1), size(map,2), 4), source=.false.)
    else
      this%timespace = .false.
    end if
    this%timespace(pij(1),pij(2),1) = .true.
  end subroutine


  subroutine onestep(this, map, dejavu)
    class(agent_t), intent(inout) :: this
    character(len=1), intent(in) :: map(:,:)
    logical, intent(out) :: dejavu
!
! Make one step and update the log (timespace)
!
    integer :: fi, fj

    rotate_loop: do
      ! tile ahead of the agent
      fi = this%pi + dirs(1,this%dr)
      fj = this%pj + dirs(2,this%dr)

      if (fi < 1 .or. fj < 1 .or. fi > size(map,1) .or. fj > size(map,2)) then
        ! map edge ahead -> leaving map
        this%pi = 0
        this%pj = 0
        return
      else
        if (map(fi,fj)=='#') then
          ! obstacle ahead -> turn right and look ahead again
          this%dr = this%dr + 1
          if (this%dr > 4) this%dr = 1
        else
          ! path free to make step forward
          this%pi = fi
          this%pj = fj
          exit rotate_loop
        end if
      end if
    end do rotate_loop
    dejavu = this%timespace(this%pi, this%pj, this%dr)
    this%timespace(this%pi, this%pj, this%dr) = .true.
  end subroutine onestep


  subroutine day2406(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: map(:,:)
    integer :: ans1, ans2, i, j, pathlen
    type(agent_t) :: agent

    map = read_pattern(file)

    ! Part 1
    call agent%measure_path(map, ans1)
    print '("Ans 06/1 ",i0,l2)', ans1, ans1==4696

    ! Part 2
    ans2 = 0
    do i=1, size(map,1)
      do j=1, size(map,2)
        if (map(i,j)/='.') cycle
        map(i,j) = '#'
        call agent%measure_path(map, pathlen)
        if (pathlen==IN_LOOP) ans2 = ans2 + 1
        map(i,j) = '.'
      end do
    end do
    print '("Ans 06/2 ",i0,l2)', ans2, ans2==1443

  end subroutine day2406

end module day2406_mod