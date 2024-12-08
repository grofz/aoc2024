module day2408_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter :: MAX_SAME_AID=5
  integer, parameter :: AID_MIN = iachar('0'), AID_MAX = iachar('z')

  type :: antennas_t 
    character(len=1), allocatable :: map(:,:)
    integer, dimension(AID_MIN:AID_MAX) :: cnt
    integer, dimension(2, MAX_SAME_AID, AID_MIN:AID_MAX) :: pos
  end type

contains

  subroutine antennas_from_file(file, this)
    character(len=*), intent(in) :: file
    type(antennas_t), intent(out) :: this

    integer :: i, j, aid

    this%map = read_pattern(file)
    this%cnt = 0
    do i=1, size(this%map,1)
    do j=1, size(this%map,2)
      associate (iach=>iachar(this%map(i,j)))
        if (iach==iachar('.')) cycle
        if (iach<lbound(this%cnt,1) .or. iach>ubound(this%cnt,1)) &
          & error stop 'this character is not expected'
        this%cnt(iach) = this%cnt(iach) + 1
        if (this%cnt(iach)>MAX_SAME_AID) &
          & error stop 'more antennas of same type than expected'
        this%pos(1, this%cnt(iach), iach) = i
        this%pos(2, this%cnt(iach), iach) = j
      end associate
    end do
    end do

    ! cross-checking the number of antennas of a particular id
    do aid = lbound(this%cnt,1), ubound(this%cnt,1)
      if (this%cnt(aid) /= count(this%map==achar(aid))) &
        & error stop 'consistency check failed'
!!    if (this%cnt(id)>0) print *, achar(aid), this%pos(:,1:this%cnt(aid),aid)
    end do
  end subroutine antennas_from_file


  subroutine count_anodes(this, ans1, ans2)
    class(antennas_t), intent(inout) :: this
    integer, intent(out) :: ans1, ans2

    integer :: aid, j, k, z
    integer, allocatable :: p1(:,:), p2(:,:)
    character(len=1), allocatable :: ano1(:,:), ano2(:,:)

    ! anodes position will be drawn to maps "ano1" and "ano2"
    allocate(ano2(size(this%map,1),size(this%map,2)), source='.')
    allocate(ano1(size(this%map,1),size(this%map,2)), source='.')

    do aid = lbound(this%cnt,1), ubound(this%cnt,1)
      A_PAIR: do j=1, this%cnt(aid)-1
      B_PAIR: do k=j+1, this%cnt(aid)
          ! find anodes for the antennas pair "j"-"k"
          associate(a=>this%pos(:,j,aid), b=>this%pos(:,k,aid))
            p1 = get_anodes_pos(a, b, shape(this%map), .false.)
            p2 = get_anodes_pos(a, b, shape(this%map), .true.)
          end associate
          ! mark anodes to the maps for Parts 1 and 2
          do z=1,size(p1,2)
            ano1(p1(1,z), p1(2,z)) = '#'
          end do
          do z = 1, size(p2,2)
            ano2(p2(1,z), p2(2,z)) = '#'
          end do
      end do B_PAIR
      end do A_PAIR
    end do
    ans1 = count(ano1=='#')
    ans2 = count(ano2=='#')
  end subroutine count_anodes


  function get_anodes_pos(a, b, siz, is_part2) result (anods)
    integer, intent(in) :: a(2), b(2)
    integer, intent(in) :: siz(2)        ! map size
    logical, intent(in) :: is_part2
    integer, allocatable :: anods(:,:)
!
! find all nodes on the line "a"-"b" within the map borders
!
    integer :: d(2), mink(2), maxk(2), k, n

    d = b-a ! vector from "a" to "b"
    if (any(d==0)) error stop 'not expecting horizontal/vertical lines'
    where (d>0)        ! "a" is bellow "b"
      mink = (a-1)/d   ! number of copies from "a" to lower border
      maxk = (siz-a)/d ! number of copies from "a" to upper border
    else where             ! reversed if "a" is above "b"
      mink = (siz-a)/(-d)  ! number of copies from "a" to upper border
      maxk = (a-1)/(-d)    ! number of copies from "a" to lower border
    end where

    ! in order to stay inside the map,
    ! we must use the minimum from "x" or "y" co-ordinates
    associate(mn=>minval(mink), mx=>minval(maxk))
      if (is_part2) then
        allocate(anods(2, mn+mx+1))
        do k = -mn, mx
          anods(:, k+mn+1) = a + d*k
        end do
      else
        ! max two anodes present for part 1
        n = 0
        if (mn>0) n = n+1 ! "k=-1"
        if (mx>1) n = n+1 ! "k= 2"
        allocate(anods(2, n))
        if (mn>0) then
          anods(:, 1) = a - d
          n = 2
        else
          n = 1
        end if
        if (mx>1) then
          anods(:, n) = a + 2*d
        end if
      end if
    end associate
  end function get_anodes_pos


  subroutine day2408(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2
    type(antennas_t) :: antennas

    call antennas_from_file(file, antennas)
    call count_anodes(antennas, ans1, ans2)
    print '("Ans 08/1 ",i0,l2)', ans1, ans1==256
    print '("Ans 08/2 ",i0,l2)', ans2, ans2==1005
  end subroutine day2408

end module day2408_mod