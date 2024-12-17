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
  end function computer_fromfile


  subroutine computer_state(this)
    class(computer_t), intent(in) :: this

    integer :: i

    write(*,*)
    print '("Regs:    ",3(i0,1x))', this%reg
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


  subroutine day2417(file)
    character(len=*), intent(in) :: file

    integer :: ans2
    type(computer_t) :: zx
    logical :: halt

    zx = computer_fromfile(file)
    do
      call zx%print()
      call zx%onestep(halt)
      if (halt) exit
    end do
    call zx%print()
    print *

    print '("Ans 17/1 ",a,l2)', trim(zx%buffer)//' ',trim(zx%buffer)=='2,1,4,0,7,4,0,2,3' 
    print '("Ans 17/2 ",i0,l2)', ans2
  end subroutine day2417

end module day2417_mod