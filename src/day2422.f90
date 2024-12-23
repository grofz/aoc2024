module day2422_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : I8 => int64

  !
  ! Simulates the sequence of prices according to the given algorithm
  !
  integer, parameter :: N_LAST_P = 4

  type pseudo_generator_t
    integer(I8) :: seed
    integer(I8) :: cnt = 0, dif(N_LAST_P) = -100
  end type
  interface pseudo_generator_t
    module procedure pseudo_generator_new
  end interface

  !
  ! For each sequence ("dif"), store the first price for each buyer
  !
  type node_ptr
    type(node_t), pointer :: ptr=>null()
  end type

  type node_t
    integer :: level ! 1, 2, 3, 4
    integer :: label ! <-9, 9>
    type(node_ptr) :: up
    type(node_ptr), dimension(-9:9) :: ch
    type(price_t), pointer :: head=>null() ! linked list of prices
  end type
  interface node_t
    module procedure node_new
  end interface

  type price_t
    integer :: price
    integer :: buyid
    type(price_t), pointer :: next=>null()
  end type
  interface price_t
    module procedure price_new
  end interface

  integer, save :: node_alloc_cnt=0, price_alloc_cnt=0

contains

  recursive subroutine traverse_sequence(root, maxtot, bestdif)
    type(node_t), pointer, intent(in) :: root
    integer, intent(out) :: maxtot, bestdif(N_LAST_P)
!
! Traverse the tree and find the sequence providing the best sum of prices
!
    integer :: dif(N_LAST_P), i, maxtot0, bestdif0(N_LAST_P)
    type(node_t), pointer :: curr

    if (.not. associated(root)) error stop 'sub-tree is empty'
    if (root%level==N_LAST_P) then
      ! at the leaf
      curr => root
      do i=N_LAST_P, 1, -1
        dif(i) = curr%label
        curr => curr%up%ptr
      end do
      bestdif = dif
      maxtot = sum_list(root%head)
    
    else
      ! select best value from all sub-trees
      maxtot = -1
      bestdif = -100
      do i=lbound(root%ch,1), ubound(root%ch,1)
        if (.not. associated(root%ch(i)%ptr)) cycle
        call traverse_sequence(root%ch(i)%ptr, maxtot0, bestdif0)
        if (maxtot0 <= maxtot) cycle
        maxtot = maxtot0
        bestdif = bestdif0
      end do
    end if
  end subroutine traverse_sequence


  integer function sum_list(head) result(tot)
    type(price_t), pointer, intent(in) :: head

    type(price_t), pointer :: curr

!!  write(*,'(a)',advance='no') 'list: '
    curr => head
    tot = 0
    do
      if (.not. associated(curr)) exit
!!    write(*,'(i0,":",i0,",  ")',advance='no') curr%price, curr%buyid
      tot = tot + curr%price
      curr => curr%next
    end do
!!  write(*,*)
  end function sum_list


  subroutine add_sequence(root, dif, price, buyid)
    type(node_t), pointer, intent(inout) :: root
    integer, intent(in) :: dif(N_LAST_P)
    integer, intent(in) :: price, buyid
!
! Construct the tree of price differences
!
    integer :: il
    type(node_t), pointer :: curr

    if (.not. associated(root)) root => node_t(root, -100)
    
    curr => root
    do il=1, 4
      if (.not. associated(curr%ch(dif(il))%ptr)) then
        curr%ch(dif(il))%ptr => node_t(curr, dif(il))
      else
      end if
      curr => curr%ch(dif(il))%ptr
    end do

    ! curr now points to the leaf of the tree
    ! only if there is no price already with the same buyid...
    if (associated(curr%head)) then
      if (curr%head%buyid == buyid) then
        return
      end if
    end if
    ! ...add the price
    curr%head => price_t(price, buyid, curr%head)
  end subroutine add_sequence


  function node_new(up, label) result(new)
    type(node_t), pointer, intent(in) :: up
    integer, intent(in) :: label
    type(node_t), pointer :: new

    allocate(new)
    new%label = label
    if (associated(up)) then
      new%level = up%level + 1
    else
      new%level = 0
    end if
    new%up%ptr => up
    new%head => null()
    node_alloc_cnt = node_alloc_cnt+1
  end function node_new


  recursive subroutine node_free(root)
    type(node_t), pointer, intent(inout) :: root

    integer :: i

    if (.not. associated(root)) return
    call price_free(root%head)
    do i=lbound(root%ch,1),ubound(root%ch,1)
      call node_free(root%ch(i)%ptr)
    end do
    deallocate(root)
    node_alloc_cnt = node_alloc_cnt - 1
  end subroutine node_free


  function price_new(price, buyid, next) result(new)
    integer, intent(in) :: price, buyid
    type(price_t), intent(in), pointer :: next
    type(price_t), pointer :: new

    allocate(new)
    new%price = price
    new%buyid = buyid
    new%next => next
    price_alloc_cnt = price_alloc_cnt+1
  end function price_new


  subroutine price_free(head)
    type(price_t), pointer, intent(inout) :: head

    type(price_t), pointer :: dele
    
    do
      if (.not. associated(head)) exit
      dele => head
      head => head % next
      deallocate(dele)
      price_alloc_cnt = price_alloc_cnt-1
    end do
  end subroutine price_free


  subroutine day2422(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer(I8) :: ans1
    integer :: buyid, j, ans2, bestdif(N_LAST_P)
    type(pseudo_generator_t) :: generator
    type(node_t), pointer :: root
    integer, parameter :: P1_REPEAT = 2000

    root => null()
    ans1 = 0
    lines = read_strings(file)
    do buyid=1, size(lines)
      generator = pseudo_generator_t(lines(buyid)%str)
      do j=1, P1_REPEAT
        call harvest(generator)
        if (all(abs(generator%dif)<=9)) then
          call add_sequence(root, int(generator%dif), last_digit(generator), buyid)
        end if
      end do
      ans1 = ans1 + generator%seed
    end do
    call traverse_sequence(root, ans2, bestdif)
    call node_free(root)
    if (node_alloc_cnt/=0 .or. price_alloc_cnt/=0) error stop 'left unallocated items'

    print '("Ans 22/1 ",i0,l2)', ans1, ans1==14476723788_I8
    print '("Ans 22/2 ",i0,l2)', ans2, ans2==1630

  end subroutine day2422


  function pseudo_generator_new(str) result(this)
    character(len=*), intent(in) :: str
    type(pseudo_generator_t) :: this

    read(str, *) this%seed
    this%cnt = 0
  end function pseudo_generator_new


  subroutine harvest(this)
    class(pseudo_generator_t) :: this

    integer :: d0

    d0 = last_digit(this)
    call mix(this, this%seed*64_I8)
    call prune(this)
    call mix(this, this%seed/32_I8)
    call prune(this)
    call mix(this, this%seed*2048_I8)
    call prune(this)
    this%cnt = this%cnt+1
    this%dif(1:N_LAST_P-1) = this%dif(2:N_LAST_P) 
    this%dif(N_LAST_P) = last_digit(this)-d0

  contains
    subroutine mix(t, b)
      class(pseudo_generator_t), intent(inout) :: t 
      integer(I8), intent(in) :: b

      t%seed = ieor(t%seed, b)
    end subroutine mix

    subroutine prune(t)
      class(pseudo_generator_t), intent(inout) :: t 
      t%seed = modulo(t%seed, 16777216_I8)
    end subroutine prune
  end subroutine harvest


  function last_digit(this) result(d)
    class(pseudo_generator_t), intent(in) :: this
    integer :: d

    d = int(modulo(this%seed, 10_I8))
  end function last_digit

end module day2422_mod