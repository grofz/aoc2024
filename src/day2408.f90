module day2408_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter :: MAX_SAME_ANTENNA=5, N_REPEATS = 80

  type :: antennas_t 
    character(len=1), allocatable :: map(:,:)
    integer, dimension(iachar('0'):iachar('z')) :: cnt
    integer, dimension(2, MAX_SAME_ANTENNA, iachar('0'):iachar('z')) :: pos 
  end type

contains

  subroutine add_anodes(this, ans1, ans2)
    class(antennas_t), intent(inout) :: this
    integer, intent(out) :: ans1, ans2

    integer :: i, j, k, z, a(2,2)
    integer, allocatable :: a1(:,:), a2(:,:), a2s(:,:)
    character(len=1), allocatable :: ano1(:,:), ano2(:,:)

    allocate(ano2(size(this%map,1),size(this%map,2)), source='.')
    allocate(ano1(size(this%map,1),size(this%map,2)), source='.')
    ANT_LOOP: do i=lbound(this%cnt,1), ubound(this%cnt,1)
      do j=1, this%cnt(i)-1
      do k=j+1, this%cnt(i)
          a = anod_pos(this%pos(:,j,i), this%pos(:,k,i))
          do z=1,2
            if (a(1,z)<1 .or. a(2,z)<1 .or. a(1,z)>size(this%map,1) .or. &
                a(2,z)>size(this%map,2)) cycle
            ano1(a(1,z),a(2,z)) = '#'
          end do
          a2 = ano_posmany(this%pos(:,j,i), this%pos(:,k,i), shape(this%map), .false.)
          a2s = ano_posmany(this%pos(:,j,i), this%pos(:,k,i), shape(this%map), .true.)
if (size(a2)/=size(a2s)) then
! print *, this%pos(:,j,i), this%pos(:,k,i)
! print '(a,*(i3))', 'IS    =', a2
! print '(a,*(i3))', 'SHOLD =', a2s
! print *
  a2 = a2s
end if
          do z=lbound(a2,2),ubound(a2,2)
!           if (a2(1,z)<1 .or. a2(2,z)<1 .or. a2(1,z)>size(this%map,1) .or. &
!               a2(2,z)>size(this%map,2)) cycle
            ano2(a2(1,z),a2(2,z)) = '#'
          end do
      end do
      end do
    end do ANT_LOOP
    ans1 = count(ano1=='#')
    ans2 = count(ano2=='#')
  end subroutine add_anodes


  function anod_pos(a, b) result(anods)
    integer, intent(in) :: a(2), b(2)
    integer :: anods(2,2)

    integer :: d(2)

    d = b-a
    anods(:,1) = b + d
    anods(:,2) = a - d
  end function anod_pos

  integer function get_mink(a, b) result(mink)
    integer, intent(in) :: a(2), b(2)
  end function

  function ano_posmany(a, b, shap, safe) result (anods)
    integer, intent(in) :: a(2), b(2), shap(2)
    integer, allocatable :: anods(:,:)
    logical, intent(in) :: safe

    integer :: d(2), k, mink, maxk, n
    integer, allocatable :: tmp(:,:)

    d = b-a
    mink = min( min(a(1),b(1))/abs(d(1))+1, min(a(2),b(2))/abs(d(2))+1)
    maxk = max(minval((shap-b)/d+1), minval((shap-a)/d+1))
    if (safe) mink = mink + N_REPEATS
    maxk = maxk + N_REPEATS
    allocate(tmp(2,mink+maxk+1))
    n = 0
    do k=-mink, maxk
      associate(q=>a+d*k)
        if (q(1)<1 .or. q(2)<1 .or. q(1)>shap(1) .or. q(2)>shap(2)) cycle
        n = n+1
        tmp(:,n) = q
      end associate
    end do
    anods = tmp(:,1:n)
  end function

  subroutine antennas_from_file(file, this)
    character(len=*), intent(in) :: file
    type(antennas_t), intent(out) :: this

    integer :: i, j

    this%map = read_pattern(file)
    this%cnt = 0
    do i=1, size(this%map,1)
    do j=1, size(this%map,2)
      associate (iach=>iachar(this%map(i,j)))
        if (iach==iachar('.')) cycle
        if (iach<lbound(this%cnt,1) .or. iach>ubound(this%cnt,1)) &
          & error stop 'character is not expected'
        this%cnt(iach) = this%cnt(iach) + 1
        if (this%cnt(iach)>MAX_SAME_ANTENNA) &
          & error stop 'more antennas than expected'
        this%pos(1, this%cnt(iach), iach) = i
        this%pos(2, this%cnt(iach), iach) = j
      end associate
    end do
    end do

    ! consistency check
    do i=lbound(this%cnt,1), ubound(this%cnt,1)
      if (this%cnt(i) /= count(this%map==achar(i))) &
        & error stop 'something went wrong'
     !if (this%cnt(i)>0) print *, achar(i), this%pos(:,1:this%cnt(i),i)
    end do
  end subroutine antennas_from_file

  subroutine day2408(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2, i
    type(antennas_t) :: antennas

    call antennas_from_file(file, antennas)
    call add_anodes(antennas, ans1, ans2)

    print '("Ans 08/1 ",i0,l2)', ans1, ans1==256
    print '("Ans 08/2 ",i0,l2)', ans2, ans2==1005

  end subroutine day2408

end module day2408_mod