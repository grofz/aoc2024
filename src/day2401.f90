module day2401_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private

  public :: day2401

contains

  subroutine day2401(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ans1, ans2

    lines = read_strings(file)
    print '("Ans 01/1 ",i0,l2)', ans1, ans1==1

  end subroutine day2401

end module day2401_mod
