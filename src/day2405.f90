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
  contains
    procedure :: middle => job_middle
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
      if (verify_page_position(root%jobs(i)%pages, root%rules)) then
        ! Part 1
        associate(x=>root%jobs(i)%middle())
          !print '("P1 ",i0)', x
          ans1 = ans1 + x
        end associate
      else
        ! Part 2
        do
          call correct_page_position(root%jobs(i)%pages,root%rules,was_ok)
          if (was_ok) exit
        end do
        associate(x=>root%jobs(i)%middle())
          !print '("P2 ",i0)', x
          ans2 = ans2 + x
        end associate
      end if
    end do
    print '("Ans 05/1 ",i0,l2)', ans1, ans1==5208
    print '("Ans 05/1 ",i0,l2)', ans2, ans2==6732

  end subroutine day2405


  function verify_page_position(pages,rules) result(isok)
    integer, intent(in) :: pages(:)
    type(rule_t), intent(in) :: rules(:)
    logical :: isok

    integer :: verified_page, i, j

    isok = .true.
    one_loop: do verified_page=1, size(pages)
      ! pages before
      do i=1, verified_page-1
        do j=1, size(rules)
          if (rules(j)%a == pages(verified_page) .and. &
              rules(j)%b == pages(i)) then
            isok = .false.
            exit one_loop
          end if
        end do
      end do
      ! pages after
      do i=verified_page+1, size(pages)
        do j=1, size(rules)
          if (rules(j)%a == pages(i) .and. &
              rules(j)%b == pages(verified_page)) then
            isok = .false.
            exit one_loop
          end if
        end do
      end do
    end do one_loop
  end function verify_page_position


  ! TODO - remove duplicit code
  ! TODO - optimize for speed
  subroutine correct_page_position(pages,rules,wasok)
    integer, intent(in out) :: pages(:)
    type(rule_t), intent(in) :: rules(:)
    logical, intent(out) :: wasok

    integer :: verified_page, i, j

    wasok = .true.
    one_loop: do verified_page=1, size(pages)
      ! pages before
      do i=1, verified_page-1
        do j=1, size(rules)
          if (rules(j)%a == pages(verified_page) .and. &
              rules(j)%b == pages(i)) then
            wasok = .false.
            call move_before(pages,verified_page,i) 
            exit one_loop
          end if
        end do
      end do
      ! pages after
      do i=verified_page+1, size(pages)
        do j=1, size(rules)
          if (rules(j)%a == pages(i) .and. &
              rules(j)%b == pages(verified_page)) then
            wasok = .false.
            call move_before(pages,i,verified_page) 
            exit one_loop
          end if
        end do
      end do
    end do one_loop
  end subroutine correct_page_position


  subroutine move_before(arr,what,where)
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: what, where

    integer :: i

    if (what < where) error stop 'what less where'
    do i=what, where+1, -1
      call swap(arr(i-1), arr(i))
    end do
  contains
    subroutine swap(a,b)
      integer, intent(inout) :: a, b
      integer :: t
      t = a
      a = b
      b = t
    end subroutine
  end subroutine


  function rule_fromstr(str) result(new)
    character(len=*), intent(in) :: str
    type(rule_t) :: new

    integer :: i
    i = scan(str,'|')
    if (i==0) error stop 'wrong format for a rule'
    read(str(:i-1),*) new%a
    read(str(i+1:),*) new%b
  end function


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
  end function 


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
  end function


  function job_middle(this) result(mid)
    class(job_t), intent(in) :: this
    integer :: mid

    integer :: i
    
    if (mod(size(this%pages),2)==0) error stop 'even has no middle'
    i = size(this%pages)/2 + 1
    mid = this%pages(i)
  end function job_middle

end module day2405_mod