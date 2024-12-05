module day2405_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type root_t
    type(rule_t), allocatable :: rules(:)
    type(job_t), allocatable :: jobs(:)
  end type
  interface root_t
    module procedure root_fromfile
  end interface

  type rule_t
    integer :: a, b
  end type rule_t
  interface rule_t
    module procedure rule_fromstr
  end interface

  type job_t
    integer, allocatable :: pages(:)
  end type job_t
  interface job_t
    module procedure job_fromstr
  end interface

contains

  subroutine day2405(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2, i
    logical :: was_ok
    type(root_t) :: root

    root = root_t(file)
    ans1 = 0
    ans2 = 0
    do i=1,size(root%jobs)
      associate(pages=>root%jobs(i)%pages)
        call correct_page_position(pages, root%rules, was_ok, .false.)
        if (was_ok) then ! Part 1
          ans1 = ans1 + middle(pages)
        else             ! Part 2
          !print '(*(i3))', root%jobs(i)%pages
          do while (.not. was_ok)
            call correct_page_position(pages, root%rules, was_ok, .true.)
            !print '(*(i3))', root%jobs(i)%pages
          end do
          ans2 = ans2 + middle(pages)
        end if
      end associate
    end do
    print '("Ans 05/1 ",i0,l2)', ans1, ans1==5208
    print '("Ans 05/1 ",i0,l2)', ans2, ans2==6732

  end subroutine day2405


  logical function is_a_lt_b(a, b, rules)
    integer, intent(in) :: a, b
    type(rule_t), intent(in) :: rules(:)

    integer :: i

    is_a_lt_b = .true. ! assume valid until proven otherwise
    do i=1,size(rules)
      if (rules(i)%b /= a .or. rules(i)%a /= b) cycle
      is_a_lt_b = .false.
      exit
    end do
  end function is_a_lt_b


  ! TODO - can be optimized for speed?
  subroutine correct_page_position(pages, rules, was_ok, fixing)
    integer, intent(in out) :: pages(:)
    type(rule_t), intent(in) :: rules(:)
    logical, intent(out) :: was_ok
    logical, intent(in) :: fixing ! try to fix it?

    integer :: verified_page, i, j

    was_ok = .true.
    one_loop: do verified_page=1, size(pages)
      ! pages before
      do i=1, verified_page-1
        if (.not. is_a_lt_b(pages(i),pages(verified_page), rules)) then
          was_ok = .false.
          if (fixing) call move_before(pages,verified_page,i)
          exit one_loop
        end if
      end do
      ! pages after
      do i=verified_page+1, size(pages)
        if (.not. is_a_lt_b(pages(verified_page),pages(i), rules)) then
          was_ok = .false.
          if (fixing) call move_before(pages,i,verified_page)
          exit one_loop
        end if
      end do
    end do one_loop
  end subroutine correct_page_position


  subroutine move_before(arr, what, where_to)
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: what, where_to
!
! Move item at the position "what" before the item at the position
! "where_to",
!
    integer :: i

    if (what < where_to) error stop 'what is less than where'
    do i=what, where_to+1, -1
      call swap(arr(i-1), arr(i))
    end do
  contains
    subroutine swap(a, b)
      integer, intent(inout) :: a, b
      integer :: t
      t = a;  a = b;  b = t
    end subroutine
  end subroutine move_before


  function middle(arr) result(mid)
    integer, intent(in) :: arr(:)
    integer :: mid
!
! Return the middle item from the array
!
    if (mod(size(arr),2)==0) error stop 'even has no middle'
    associate (i=>size(arr)/2 + 1)
      mid = arr(i)
    end associate
  end function middle


  ! =================
  ! Parsing the input
  ! =================

  function rule_fromstr(str) result(new)
    character(len=*), intent(in) :: str
    type(rule_t) :: new

    integer :: i
    i = scan(str,'|')
    if (i==0) error stop 'wrong format for a rule'
    read(str(:i-1),*) new%a
    read(str(i+1:),*) new%b
  end function rule_fromstr


  function job_fromstr(str) result(new)
    character(len=*), intent(in) :: str
    type(job_t) :: new

    type(string_t), allocatable :: items(:)
    integer :: i

    call split(str, ',', items)
    allocate(new%pages(size(items)))
    do i=1, size(items)
      read(items(i)%str,*) new%pages(i)
    end do
  end function job_fromstr


  function root_fromfile(file) result(new)
    character(len=*), intent(in) :: file
    type(root_t) :: new

    type(string_t), allocatable :: lines(:)
    integer :: i, iphase

    lines = read_strings(file)
    allocate(new%jobs(0), new%rules(0))
    iphase = 1
    do i=1, size(lines)
      if (lines(i)%str=='') then
        if (iphase==1) then
          iphase=2
          cycle
        else
          error stop 'two empty lines'
        end if
      end if
      if (iphase==1) then
        new%rules = [new%rules, rule_t(lines(i)%str)]
      else if (iphase==2) then
        new%jobs = [new%jobs, job_t(lines(i)%str)]
      end if
    end do
  end function root_fromfile

end module day2405_mod
