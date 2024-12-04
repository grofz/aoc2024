module day2404_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter, dimension(2,8) :: dir = reshape( &
    [-1, -1, &
      0, -1, &
      1, -1, &
      1,  0, &
      1,  1, &
      0,  1, &
     -1,  1, &
     -1,  0  ], [2,8])

contains

  subroutine day2404(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2, i, j, k
    character(len=1), allocatable :: map(:,:)

    map = read_pattern(file)
    ans1 = 0
    ans2 = 0
    do i=1,size(map,1)
      do j=1,size(map,2)
        do k=1,size(dir,2)
          if (verify_p1(map,i,j,k)) ans1 = ans1+1
        end do
        if (verify_p2(map,i,j)) ans2 = ans2+1
      end do
    end do

    print '("Ans 04/1 ",i0,l2)', ans1, ans1==2401
    print '("Ans 04/2 ",i0,l2)', ans2, ans2==1822
  end subroutine day2404


  logical function verify_p1(map, pi, pj, d) result(isok)
    character(len=1), intent(in) :: map(:,:)
    integer, intent(in) :: pi, pj, d
!
! Return .T. if from position [pi,pj] in the direction "d" one
! can read the "XMAS"
! 
    character(len=*), parameter :: word = 'XMAS'
    integer :: iletter, i, j

    do iletter = 1, len(word)
      i = pi + (iletter-1)*dir(1,d)
      j = pj + (iletter-1)*dir(2,d)
      if (i<1 .or. j<1 .or. i>size(map,1) .or. j>size(map,2)) exit
      if (map(i,j) /= word(iletter:iletter)) exit
    end do
    isok = iletter==len(word)+1
  end function


  logical function verify_p2(map, pi, pj) result(isok)
    character(len=1), intent(in) :: map(:,:)
    integer, intent(in) :: pi, pj

!
! Return .T. if the position [pi,pj] is the center of
! crossing MAS words.
!
    integer :: iletter, i, j, cnt_m(2), cnt_s(2), diri, dirj

    if (map(pi,pj) /= 'A') then
      isok = .false.
      return
    end if

    cnt_m = 0
    cnt_s = 0

    ! loop over NE / NW / SE / SW positions from [pi,pj]
    do diri=-1, 1, 2
      do dirj=-1, 1, 2
        i = pi+diri
        j = pj+dirj
        if (i<1 .or. j<1 .or. i>size(map,1) .or. j>size(map,2)) cycle
        if (map(i,j)=='M') then
          cnt_m(1) = cnt_m(1) + 1
          ! is this "M" on a SW-NE diagonal?
          if (diri+dirj==0) cnt_m(2) = cnt_m(2) + 1
        end if
        if (map(i,j)=='S') then
          cnt_s(1) = cnt_s(1) + 1
          ! is this "S" on a SW-NE diagonal?
          if (diri+dirj==0) cnt_s(2) = cnt_s(2) + 1
        end if
      end do
    end do
    ! Two "M" and "S" must be found, one "M" and "S" must be on SW-NE diagonal
    isok = cnt_m(1)==2 .and. cnt_s(1)==2 .and. cnt_m(2)==1 .and. cnt_s(2)==1
  end function

end module day2404_mod