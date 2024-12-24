module day2424_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I8 => int64
  implicit none

  integer, parameter :: WLAB_LEN = 3, VAL_UNDEF = -1

  type wire_t 
    character(len=WLAB_LEN) :: lab
    integer :: wid
    integer :: val = VAL_UNDEF
  end type

  integer, parameter :: TYP_AND=1, TYP_OR=2, TYP_XOR=3
  type gate_t
    integer :: typ
    integer :: wa, wb, wc
  end type

contains

  subroutine evaluate_gates(gates, wires)
    type(gate_t), intent(in) :: gates(:)
    type(wire_t), intent(inout) :: wires(:)

    logical :: defined(size(gates))
    integer :: i
    logical :: xa, xb, xc

    defined = .false.
    MAIN: do 
print *, 'unevaluated gates = ', count(.not. defined)
      if (all(defined)) exit MAIN
      do i=1, size(gates)
        ! skip if already evaluated
        if (defined(i)) cycle 
        ! skip if any inputs is yet unknown
        if (wires(gates(i)%wa)%val == VAL_UNDEF .or. &
            wires(gates(i)%wb)%val == VAL_UNDEF) cycle
        
        ! gate can be evaluated
        xa = wires(gates(i)%wa)%val == 1
        xb = wires(gates(i)%wb)%val == 1

        select case(gates(i)%typ)
        case(TYP_AND)
          xc = xa .and. xb 
        case(TYP_OR)
          xc = xa .or. xb 
        case(TYP_XOR)
          xc = xa .neqv. xb 
        case default
          error stop 'unknown gate type'
        end select

        ! update wire value
        if (xc) then
          wires(gates(i)%wc)%val = 1
        else
          wires(gates(i)%wc)%val = 0
        end if
        defined(i) = .true.
      end do

    end do MAIN
  end subroutine evaluate_gates


  subroutine connect_gate(str, gate, wires, nw)
    character(len=*), intent(in) :: str
    type(gate_t), intent(out) :: gate
    type(wire_t), allocatable, intent(inout) :: wires(:)
    integer, intent(inout) :: nw

    type(string_t), allocatable :: toks(:)
    integer :: wid

    call split(str, ' ', toks)
    if (size(toks)/=5) error stop 'connect_gate - expecting 5 tokens'
    if (toks(4)%str /= '->') error stop 'connect gate - format invalid'

    if (toks(2)%str=='AND') then
      gate%typ = TYP_AND
    else if (toks(2)%str=='OR') then
      gate%typ = TYP_OR
    else if (toks(2)%str=='XOR') then
      gate%typ = TYP_XOR
    else
      error stop 'connect_gate - only AND, OR, or XOR gates supported'
    end if

    call add_wire(wires, nw, toks(1)%str, wid)
    gate%wa = wid
    call add_wire(wires, nw, toks(3)%str, wid)
    gate%wb = wid
    call add_wire(wires, nw, toks(5)%str, wid)
    gate%wc = wid
  end subroutine connect_gate


  subroutine read_wire(str, wires, nw)
    character(len=*), intent(in) :: str
    type(wire_t), allocatable, intent(inout) :: wires(:)
    integer, intent(inout) :: nw

    integer :: wid, val

    if (str(4:5)/=': ') error stop 'read_wire - invalid format'
    select case (str(6:6))
    case('0')
      val = 0
    case('1')
      val = 1
    case default
      error stop 'read_wire - only 0 or 1 allowed'
    end select
    call add_wire(wires, nw, str(1:3), wid, val=val)
  end subroutine read_wire


  subroutine add_wire(wires, nw, wlab, wid, val)
    type(wire_t), allocatable, intent(inout) :: wires(:)
    integer, intent(inout) :: nw
    character(len=WLAB_LEN), intent(in) :: wlab
    integer, intent(out) :: wid
    integer, intent(in), optional :: val

    type(wire_t), allocatable :: tmp(:)
    integer :: i

    ! make sure, there is a space for a new wire
    if (.not. allocated(wires)) then
      allocate(wires(8))
      nw = 0
    end if
    if (nw == size(wires)) then
      allocate(tmp(2*nw))
      tmp(1:nw) = wires
      call move_alloc(tmp, wires)
    end if

    ! check if the wire already exists
    do i=1, nw
      if (wires(i)%lab == wlab) exit
    end do
    wid = i

    ! if not found, add new wire
    if (wid==nw+1) then
      nw = nw + 1
      wires(wid)%lab = wlab
      wires(wid)%wid = wid
      wires(wid)%val = VAL_UNDEF
    end if

    ! set wire
    if (present(val)) wires(wid)%val = val
  end subroutine add_wire


  function read_output(wires, nw, ch) result(ans)
    type(wire_t), intent(in) :: wires(:)
    integer, intent(in) :: nw
    character(len=1), intent(in) :: ch
    integer(I8) :: ans

    integer :: i, j

    ! clear all the bits
    ans = 0
    do i=0, bit_size(ans)-1
      ans = ibclr(ans, i)
    end do

    do j=1, nw
      if (wires(j)%lab(1:1)/=ch) cycle
      read(wires(j)%lab(2:3),*) i
      if (wires(j)%val==1) ans = ibset(ans, i)
    end do
  end function read_output


  function i2b(inp) result(bin)
    integer(I8), intent(in) :: inp
    character(len=bit_size(inp)) :: bin

    integer :: i
    character(len=1) :: bd

    do i=0, bit_size(inp)-1
      bd = '0'
      if (ibits(inp, i, 1)==1) bd = '1'
      bin(bit_size(inp)-i:bit_size(inp)-i) = bd
    end do
  end function i2b


  subroutine day2424(file)
    character(len=*), intent(in) :: file

    integer(I8) :: ans1
    type(wire_t), allocatable :: wires(:)
    type(gate_t), allocatable :: gates(:)
    type(string_t), allocatable :: lines(:)
    integer :: nw, i, j

    ! Parse wires and gates
    lines = read_strings(file)
    do i=1, size(lines)
      if (len(lines(i)%str)==0) exit
      call read_wire(lines(i)%str, wires, nw)
    end do
    if (i==size(lines)+1) error stop 'day24 - no empty line in the input'

    allocate(gates(size(lines)-i))
    do j = i+1, size(lines)
      call connect_gate(lines(j)%str, gates(j-i), wires, nw)
    end do
    print '("Found ",i0," wires and ",i0," gates in the input file")', nw, size(gates) 

    ! Evaluate network for Part 1
    call evaluate_gates(gates, wires)
    ans1 = read_output(wires, nw, 'z')

    print '(i0," + ",i0," = ",i0)', &
      read_output(wires,nw,'x'), read_output(wires,nw,'y'), read_output(wires,nw,'z')
    print '(i0," + ",i0," = ",i0)', &
      read_output(wires,nw,'x'), read_output(wires,nw,'y'), read_output(wires,nw,'x')+read_output(wires,nw,'y')
    print *, i2b(read_output(wires,nw,'z')) 
    print *, i2b(read_output(wires,nw,'x')+read_output(wires,nw,'y')) 

    print '("Ans 24/1 ",i0,l2)', ans1, ans1==64755511006320_I8
  end subroutine day2424
end module day2424_mod