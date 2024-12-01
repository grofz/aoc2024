module day2402_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public :: day2402

contains

  subroutine day2402(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ans1, ans2, i

   !lines = read_strings(file)

    ! Part 1
    print '("Ans 02/1 ",i0,l2)', ans1, ans1==3569916

    ! Part 2
    print '("Ans 02/2 ",i0,l2)', ans2, ans2==26407426

  end subroutine day2402

end module day2402_mod