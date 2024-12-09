module day2409_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : I8 => int64
  implicit none

  type block_t
    integer :: id
    integer :: free_after = 0
    integer :: len=1 
    type(block_t), pointer :: next => null(), prev => null()
  end type
  interface block_t
    module procedure new_block
  end interface

  integer :: alloc_counter = 0

contains

  subroutine day2409(file)
    character(len=*), intent(in) :: file

    type(block_t), pointer :: head, tail
    integer(i8) :: ans1, ans2

    ! Part 1
    call new_disc(file, head, tail, .false.)
    call defragment_disc(head, tail, .false.)
    ans1 = checksum(head)
    call free_blocks(head)

    ! Part 2
    call new_disc(file, head, tail, .true.)
    call defragment_disc(head, tail, .true.)
    ans2 = checksum(head)
    call free_blocks(head)

    print '("Ans 09/1 ",i0,l2)', ans1, ans1==6399153661894_i8
    print '("Ans 09/2 ",i0,l2)', ans2, ans2==6421724645083_i8
  end subroutine


  function new_block(id, free_after, len) result(new)
    integer, intent(in) :: id, free_after
    integer, intent(in), optional :: len
    type(block_t), pointer :: new

    allocate(new)
    new%id = id
    new%free_after = free_after
    if (present(len)) new%len=len
    alloc_counter = alloc_counter+1
  end function new_block


  subroutine new_disc(file, head, tail, is_part2)
    character(len=*), intent(in) :: file
    type(block_t), pointer, intent(out) :: tail, head
    logical, intent(in) :: is_part2

    type(string_t), allocatable :: line(:)
    integer :: i, j, id, nblocks
    type(block_t), pointer :: blok

    head => null()
    tail => null()
    line = read_strings(file)
    if (size(line)/=1) error stop 'just single line expected'
    if (mod(len_trim(line(1)%str), 2)==0) error stop 'size is even'

    id = 0
    do i=1, len_trim(line(1)%str), 2
      read(line(1)%str(i:i), *) nblocks
      if (nblocks==0) error stop 'ghost (zero size) block'
      if (is_part2) then
        blok => block_t(id, 0, nblocks)
        blok%prev => tail
        if (associated(tail)) tail%next => blok
        if (.not. associated(tail)) head => blok
        tail => blok
      else
        do j=1, nblocks
          ! add one-size blocks 
          blok => block_t(id, 0)
          blok%prev => tail
          if (associated(tail)) tail%next => blok
          if (.not. associated(tail)) head => blok
          tail => blok
        end do
      end if
      ! add free-space after last block
      if (i+1 < len_trim(line(1)%str))then
        read(line(1)%str(i+1:i+1), *) tail%free_after
      end if
      if (is_part2) then
      else
      end if
      id = id+1
    end do
  end subroutine new_disc


  subroutine defragment_disc(head, tail, is_part2)
    type(block_t), pointer, intent(in) :: head
    type(block_t), pointer, intent(inout) :: tail
    logical, intent(in) :: is_part2

    type(block_t), pointer :: ahead_of_free, moved
    integer :: id

    if (is_part2) then
      ID_LOOP: do id = tail%id, 1, -1
        ! point "tail" to the block that could be moved
        do
          if (tail%id == id) exit
          tail => tail%prev
          if (.not. associated(tail)) error stop 'block not found'
        end do

        ! find free space where the block could be moved
        ahead_of_free => head
        do
          if (ahead_of_free%free_after >= tail%len) exit
          ahead_of_free => ahead_of_free%next
          ! stop search if reaching blok to be moved, or the end of list
          if (.not. associated(ahead_of_free)) cycle ID_LOOP
          if (ahead_of_free%id == id) cycle ID_LOOP
        end do

        ! we know that space exists, move block after "ahead_of_free"
        moved => tail
        tail => tail%prev
        ! unlink "moved"
        tail%next => moved%next
        if (associated(tail%next)) tail%next%prev => tail
        tail%free_after = tail%free_after + moved%len + moved%free_after

        ! insert "moved" after "ahead_of_free"
        moved%next => ahead_of_free%next
        moved%next%prev => moved
        ahead_of_free%next => moved
        moved%prev => ahead_of_free

        moved%free_after = ahead_of_free%free_after - moved%len
        ahead_of_free%free_after = 0
      end do ID_LOOP

    else 
      ! part 1 defragmentation rules
      ahead_of_free => head
      MAIN_LOOP: do
        do ! until free space (or end of list) reached
          if (ahead_of_free%free_after>0) exit
          if (.not. associated(ahead_of_free%next)) exit
          ahead_of_free => ahead_of_free%next
        end do
        if (ahead_of_free%free_after==0) exit MAIN_LOOP

        ! move last block forward
        moved => tail
        tail => moved%prev
        if (.not. associated(tail)) error stop 'tail went too before'
        tail%next => null()
        if (associated(moved%next)) error stop 'moved not the last one'
        moved%prev => ahead_of_free
        moved%next => ahead_of_free%next
        moved%next%prev => moved
        ahead_of_free%next => moved

        moved%free_after = ahead_of_free%free_after-1
        ahead_of_free%free_after = 0
      end do MAIN_LOOP
    end if
  end subroutine defragment_disc


  function checksum(head) result(ans)
    type(block_t), pointer, intent(in) :: head
    integer(i8) :: ans

    type(block_t), pointer :: current
    integer :: ip, j

    ans = 0
    ip = 0
    current => head
    do
      if (.not. associated(current)) exit
      do j=1, current%len
        ans = ans + ip*current%id
        ip = ip+1
      end do
      ip = ip + current%free_after
      current => current%next
    end do
  end function checksum


  subroutine free_blocks(head)
    type(block_t), pointer, intent(inout) :: head

    type(block_t), pointer :: to_delete

    do while(associated(head))
      to_delete => head
      head => head%next
      deallocate(to_delete)
      alloc_counter = alloc_counter-1
    end do
    if (alloc_counter/=0) error stop 'leaking memory'
  end subroutine free_blocks

end module day2409_mod