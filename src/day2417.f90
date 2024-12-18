module day2417_mod
  use iso_fortran_env, only : i8 => int64
  use parse_mod, only : string_t, read_strings, split
  implicit none

  integer, parameter :: MAX_BUFFER = 100
  integer, parameter :: REG_A=0, REG_B=1, REG_C=2, OP_ADV=0, OP_BXL=1, &
      OP_BST=2, OP_JNZ=3, OP_BXC=4, OP_OUT=5, OP_BDV=6, OP_CDV=7
  type computer_t
    integer(i8) :: reg(0:2)
    integer,  allocatable :: program(:)
    integer :: ip = 0
    character(len=MAX_BUFFER) :: buffer=''
    integer(i8), allocatable :: output(:)
  contains
    procedure :: print => computer_state
    procedure :: onestep => process_instruction
  end type

contains

  function computer_fromfile(file) result(this)
    character(len=*), intent(in) :: file
    type(computer_t) :: this

    type(string_t), allocatable :: lines(:), program(:)
    character(len=*), parameter :: key1 = "Register A:"
    character(len=*), parameter :: key2 = "Register B:"
    character(len=*), parameter :: key3 = "Register C:"
    character(len=*), parameter :: key4 = "Program:"
    integer :: i

    lines = read_strings(file)
    if (size(lines)/=5) error stop 'expecting 5 lines, our expectation not met'
    associate(f=>lines(1)%str)
      i = index(f, key1)
      if (i==0) error stop 'do not recognize first line'
      read(f(i+len(key1):), *) this%reg(REG_A)
    end associate
    associate(f=>lines(2)%str)
      i = index(f, key2)
      if (i==0) error stop 'do not recognize second line'
      read(f(i+len(key2):), *) this%reg(REG_B)
    end associate
    associate(f=>lines(3)%str)
      i = index(f, key3)
      if (i==0) error stop 'do not recognize third line'
      read(f(i+len(key3):), *) this%reg(REG_C)
    end associate
    associate(f=>lines(5)%str)
      i = index(f, key4)
      if (i==0) error stop 'do not recognize last line'
      call split(f(i+len(key4):), ',', program)
    end associate
    allocate(this%program(0:size(program)-1))
    do i=1, size(program)
      read(program(i)%str,*) this%program(i-1)
    end do
    this%ip = 0
    allocate(this%output(0))
  end function computer_fromfile


  subroutine computer_state(this)
    class(computer_t), intent(in) :: this

    integer :: i

    write(*,*)
    print '("Regs:    ",3(i0,1x))', this%reg
 print '(a)', trim(view_binary(this%reg(REG_A),'A'))
 print '(a)', trim(view_binary(this%reg(REG_B),'B'))
 print '(a)', trim(view_binary(this%reg(REG_C),'C'))
    write(*,'("Program: ")',advance='no')
    do i=0, this%ip-1
      write(*,'(*(i0,1x))',advance='no') this%program(i)
    end do
    write(*,'(" * ")', advance='no')
    do i=this%ip, size(this%program)-1
      write(*,'(*(i0,1x))',advance='no') this%program(i)
    end do
    write(*,*)
    write(*,'("Buffer:  ",a)') trim(this%buffer)
  end subroutine computer_state


  function operand(this, combo) result(op)
    class(computer_t), intent(in) :: this
    integer, intent(in) :: combo
    integer(i8) :: op

    if (combo >= 0 .and. combo <= 3) then
      op = int(combo, kind=i8)
    else if (combo < 7) then
      op = this%reg(combo-4)
    else if (combo == 7) then
      error stop 'this combo not yet implemented'
    else
      error stop 'combo out of range'
    end if
  end function operand


  subroutine process_instruction(this, halt)
    class(computer_t), intent(inout) :: this
    logical, intent(out) :: halt

    integer :: opcode

    if (this%ip > size(this%program)) then
      print *, 'end of program'
      stop 888
    end if

    opcode = this%program(this%ip)
    select case(opcode)
    case(OP_ADV)
      this%reg(REG_A) = &
          this%reg(REG_A) / (2**operand(this, this%program(this%ip+1)))
    case(OP_BXL)
      this%reg(REG_B) = &
          ieor(this%reg(REG_B), int(this%program(this%ip+1), kind=i8))
    case(OP_BST)
      this%reg(REG_B) = &
          modulo(operand(this, this%program(this%ip+1)), 8)
    case(OP_JNZ)
      if (this%reg(REG_A)/=0) then
        this%ip=this%program(this%ip+1)
      else
        this%ip = this%ip + 2
      end if
    case(OP_BXC)
      this%reg(REG_B) = ieor(this%reg(REG_B), this%reg(REG_C))
    case(OP_OUT)
      ! add comma after last number
      if (len_trim(this%buffer)/=0) then
        write(this%buffer(len_trim(this%buffer)+1:),'(a)') ','
      end if
      ! write (OP mod 8)
      write(this%buffer(len_trim(this%buffer)+1:),'(i0)') &
          & modulo(operand(this,this%program(this%ip+1)), 8)
      ! add to an array
      this%output = [this%output, &
          & int(modulo(operand(this,this%program(this%ip+1)), 8), i8) ]
    case(OP_BDV)
      this%reg(REG_B) = &
          this%reg(REG_A) / (2**operand(this, this%program(this%ip+1)))
    case(OP_CDV)
      this%reg(REG_C) = &
          this%reg(REG_A) / (2**operand(this, this%program(this%ip+1)))
    case default
      error stop 'invalid opcode'
    end select

    if (opcode/=OP_JNZ) this%ip = this%ip + 2
    halt = this%ip >= size(this%program)
  end subroutine


  function view_binary(num, reg) result(str)
    integer(i8), intent(in) :: num
    character(len=1), intent(in) :: reg
    character(len=87) :: str

    integer :: i

    str = ''
    str(1:1) = reg
    do i=0, bit_size(num)-1
      if (btest(num,i)) then
        str(3+i:3+i) = '1'
      else
        str(3+i:3+i) = '0'
      end if
    end do
    write(str(bit_size(num)+3+1:), '("(",i0,")")') num
  end function view_binary


  function simulate(zx, regA, output)
    type(computer_t), intent(in) :: zx
    integer(i8), intent(in) :: regA
    character(len=:), allocatable :: simulate
    integer(i8), intent(out), allocatable, optional :: output(:)

    type(computer_t) :: zx0
    logical :: halt

    zx0 = zx
    zx0%reg(REG_A) = regA
    do
      call zx0%onestep(halt)
      if (halt) exit
    end do
    simulate = zx0%buffer
    if (present(output)) output = zx0%output
  end function


  subroutine searchlast(zx, goal, pool, good)
    type(computer_t) ,intent(in) :: zx
    integer(i8), intent(in) :: goal(:)
    integer(i8), intent(in) :: pool(:)
    integer(i8), allocatable, intent(out) :: good(:)

    ! to bootstrap - put just [0] to pool

    integer(i8), allocatable :: wrk(:), output(:), regA
    integer :: ngood, i, k, maxk
    character(len=:), allocatable :: buffer

    ngood = 0
    if (size(pool)==1 .and. pool(1)==0) then
      maxk = 2**10-1
    else
      maxk = 2**3-1
    end if
    do i=1, size(pool)
      do k=0, maxk
        ! shift by three bits to the right and test all bits
        ! before
        regA = pool(i)*int(8,i8) + int(k,i8)

        buffer=simulate(zx, regA, output)
        if (size(output)>=size(goal)) then
          if (.not. all(goal==output(size(output)-size(goal)+1:))) cycle
          call safe_add(regA)
        end if
      end do
    end do

    ! export good values
    good = wrk(1:ngood)
    print '("From pool of ",i0," good values remain ",i0)', size(pool), size(good)

  contains
    subroutine safe_add(ii)
      integer(i8), intent(in) :: ii

      integer(i8), allocatable :: tmp(:)

      if (.not. allocated(wrk)) then
        allocate(wrk(8))
        ngood = 0
      end if
      if (ngood==size(wrk)) then
        allocate(tmp(2*ngood))
        tmp(1:ngood) = wrk
        call move_alloc(tmp,wrk)
      end if
      ngood = ngood + 1
      wrk(ngood) = ii
    end subroutine
  end subroutine searchlast


  subroutine day2417(file)
    character(len=*), intent(in) :: file

    integer(i8) :: ans2, i, j
    integer(i8), allocatable :: good(:), good0(:), output(:)
    character(len=:), allocatable :: buffer
    type(computer_t) :: zx, zx2
    logical :: halt

    zx = computer_fromfile(file)
    do
!     call zx%print()
      call zx%onestep(halt)
      if (halt) exit
    end do

    zx2 = zx
    zx = computer_fromfile(file)
    good0 = [0_i8]
    do i=1, size(zx%program)
      call searchlast(zx, int(zx%program(size(zx%program)-i:size(zx%program)-1), i8), good0, good)
      good0 = good
    end do

    ans2 = huge(ans2)
    do i=1,size(good)
      buffer = simulate(zx, good(i), output)
      if (size(output)==size(zx%program)) then
        if (all(output==zx%program) .and. ans2>good(i)) then
          ans2 = good(i)
        end if
      end if
    end do

    print '("Ans 17/1 ",a,l2)', trim(zx2%buffer)//' ',trim(zx2%buffer)=='2,1,4,0,7,4,0,2,3' 
    print '("Ans 17/2 ",i0,l2)', ans2, ans2==258394985014171_i8
  end subroutine day2417

end module day2417_mod
