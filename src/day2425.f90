module day2425_mod
  use parse_mod, only : read_pattern, string_t, read_strings
  implicit none

  integer, parameter, dimension(2) :: MAP_SIZE = [7,5]

contains

  subroutine day2425(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    character(len=1), allocatable :: plans(:,:,:)
    logical, allocatable :: islock(:)
    integer :: i, j, ans

    lines = read_strings(file)
    if (mod(size(lines)+1, MAP_SIZE(1)+1)/=0) error stop 'wrong number of lines in input'
    allocate(plans(MAP_SIZE(1), MAP_SIZE(2), (size(lines)+1)/(MAP_SIZE(1)+1)))
    allocate(islock(size(plans,3)))

    do i=1, size(plans,3)
      call read_plan( &
        lines((i-1)*(MAP_SIZE(1)+1)+1 : i*(MAP_SIZE(1)+1)-1),  plans(:,:,i), islock(i) )
    end do

    ans = 0
    do i=1, size(plans,3)
    do j=1, size(plans,3)
      if (i==j) cycle
      if ((.not. islock(i)) .or. islock(j)) cycle
      if (fit(plans(:,:,i), plans(:,:,j))) ans = ans+1
    end do
    end do
    print '("Ans 25/1 ",i0,l2)', ans, ans==3439

  end subroutine day2425


  subroutine read_plan(lines, plan, islock)
    type(string_t), intent(in) :: lines(:)
    character(len=1), intent(out) :: plan(:,:)
    logical, intent(out) :: islock

    integer :: i, j, ntop, nbot

    if (size(lines)/=size(plan,1)) error stop 'inconsistent lines/plan'
    do i=1, size(lines)
      if (len(lines(i)%str)/=size(plan,2)) error stop 'line length inconsistent'
      do j=1, size(plan,2)
        plan(i,j) = lines(i)%str(j:j)
      end do
    end do

    ntop = count(plan(1,:)=='#')
    nbot = count(plan(size(plan,1),:)=='#')
    if (ntop==size(plan,2) .and. nbot==0) then
      ! locks have top row filled by '#' and bottom row empty '.'
      islock = .true.
    else if (ntop==0 .and. nbot==size(plan,2) ) then
      ! keys have bottom row filled by '#' and top row empty '.'
      islock = .false.
    else
      error stop 'lock/key identification failed'
    end if
  end subroutine read_plan


  function fit(lock, key) result(yes)
    character(len=1), intent(in) :: lock(:,:), key(:,:)
    logical :: yes

    associate( &
       nl => count(lock(2:size(lock,1)-1, :)=='#', dim=1), &
       nk => count(key (2:size(key,1)-1,  :)=='#', dim=1) )
       yes = .not. any(nl+nk > size(lock,1)-2)
    end associate
  end function fit

end module day2425_mod