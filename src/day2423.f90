module day2423_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  integer, parameter :: VNAME_LEN=2

  type edge_t
    character(len=VNAME_LEN) :: v1, v2
    integer :: idv2
    type(edge_t), pointer :: next
  end type
  interface edge_t
    module procedure new_edge
  end interface

  integer, save :: edge_alloc_cnt = 0

  type vertex_t
    character(len=VNAME_LEN) :: label
    type(edge_t), pointer :: adjlist=>null()
  contains
    !final :: free_vertices ! gfortran segfaults
  end type

contains

  subroutine day2423(file)
    character(len=*), intent(in) :: file

    block
      type(vertex_t), allocatable :: varr(:)
      integer :: nv, i, ans1
      integer, allocatable :: r(:), p(:), x(:), maxcliq(:)
      character(len=:), allocatable :: ans2

      call build_varr(file, varr, nv)
print *, 'Part 1'
      call triplets(varr(1:nv), ans1)

print *, 'Part 2'
      allocate(r(0), x(0))
      p = [(i, i=1, nv)]
      call broker(r, p, x, varr(1:nv), maxcliq)
      ans2 = sort_labels(varr, maxcliq)

      print '("Ans 23/1 ",i0,l2)', ans1, ans1==1302
      print '("Ans 23/2 ",a,l2)', ans2, ans2=='cb,df,fo,ho,kk,nw,ox,pq,rt,sf,tq,wi,xz'

      call free_vertices(varr)
    end block
    if (edge_alloc_cnt/=0) error stop 'leaking memory'

  end subroutine day2423


  function sort_labels(varr, maxcliq) result(str)
    type(vertex_t), intent(in) :: varr(:)
    integer, intent(in) :: maxcliq(:)
    character(len=:), allocatable :: str

    character(len=VNAME_LEN), allocatable :: labels(:)
    character(len=VNAME_LEN) :: key
    integer :: i, j

    allocate(labels(size(maxcliq)))
    do i=1, size(maxcliq)
      labels(i) = varr(maxcliq(i))%label
    end do

    ! selection sort
    do i = 2, size(labels) 
      key = labels(i)
      j = i-1
      do 
        if (j<1) exit
        if (labels(j) <= key) exit
        labels(j+1) = labels(j)
        j = j-1
      end do
      labels(j+1) = key
    end do

    ! format output
    allocate(character(len=size(labels)*(VNAME_LEN+1)-1) :: str)
    do i=1, size(labels)
      j = (VNAME_LEN+1)*(i-1)+1
      str(j:j+1) = labels(i)
      if (i/=size(labels)) str(j+2:j+2) = ','
    end do
  end function sort_labels


  !
  ! Bron Kerbosh algorithm
  !
  recursive subroutine broker(r, p, x, varr, maxcliq)
    integer, intent(in) :: r(:), p(:), x(:)
    type(vertex_t), intent(in) :: varr(:)
    integer, allocatable, intent(out) :: maxcliq(:)
!
! Bron-Kerbosh algorithm
!
! https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
! https://www.youtube.com/watch?v=j_uQChgo72I
!
    integer, allocatable :: p0(:), x0(:), maxcliq0(:)
    integer :: i

    if (size(p)==0 .and. size(x)==0) then
      maxcliq = r
!!    print *, r
    else
      allocate(maxcliq(0))
      p0 = p
      x0 = x
      do i=1, size(p)
        call broker( &
            & [r, p0(1)], &
            & intersection_ngb(p0, p0(1), varr), &
            & intersection_ngb(x0, p0(1), varr), varr, maxcliq0 )
        x0 = [x0, p0(1)]
        p0 = p0(2:size(p0))
        if (size(maxcliq0)>size(maxcliq)) maxcliq = maxcliq0
      end do
    end if
  end subroutine broker


  function intersection_ngb(setA, v, varr) result(set)
    integer, intent(in) :: setA(:)
    integer, intent(in) :: v
    type(vertex_t), intent(in) :: varr(:)
    integer, allocatable :: set(:)
!
! Return the intersection of "setA" with the neighborough of vertex "v"
!
    integer, allocatable :: wrk(:), ngb(:)
    integer :: i, n

    ngb = export_list_ids(varr(v)%adjlist)
    allocate(wrk(size(ngb)))
    n = 0
    do i=1, size(ngb)
      if (findloc(setA, ngb(i), dim=1)==0) cycle
      n = n+1
      wrk(n) = ngb(i)
    end do
    allocate(set(n))
    set = wrk(1:n)
  end function intersection_ngb



  ! 
  ! triplets for Part 1 - naive brute force
  ! TODO - also broker can be used to find tree-node cliques
  !
  subroutine triplets(varr, ans1)
    type(vertex_t), intent(in) :: varr(:)
    integer, intent(out) :: ans1

    integer :: i, j, k
    type(edge_t), pointer :: is_ij, is_ik, is_jk

    ans1 = 0
    do i=1, size(varr)-2
      do j=i+1, size(varr)-1
        do k=j+1, size(varr)
          is_ij => search_list(varr(i)%adjlist, varr(j)%label)
          is_ik => search_list(varr(i)%adjlist, varr(k)%label)
          is_jk => search_list(varr(j)%adjlist, varr(k)%label)
          if (associated(is_ij) .and. associated(is_ik) .and. associated(is_jk)) then
            ! triplet found
            if (varr(i)%label(1:1)=='t' .or. varr(j)%label(1:1)=='t' .or. varr(k)%label(1:1)=='t') then
!!            print '(3(a2,","))', varr(i)%label, varr(j)%label, varr(k)%label
              ans1 = ans1 + 1
            else
!!            print '(3(a2,","))', varr(i)%label, varr(j)%label, varr(k)%label
            end if
          end if
        end do
      end do
    end do
  end subroutine triplets


  !
  ! Construct the list of vertices and adjacency lists
  !
  subroutine build_varr(file, varr, nv)
    character(len=*), intent(in) :: file
    type(vertex_t), allocatable, intent(out) :: varr(:)
    integer, intent(out) :: nv

    type(string_t), allocatable :: lines(:)
    integer :: i, id1, id2
    type(edge_t), pointer :: edge

    lines = read_strings(file)
    do i=1, size(lines)
      associate(str=>lines(i)%str)
        if (len(str)/=5) error stop 'just 5 characters expected'
        if (str(3:3)/='-') error stop 'dash expected in the middle'
        call add_vertex(varr, nv, str(1:2), id1)
        call add_vertex(varr, nv, str(4:5), id2)
        edge => search_list(varr(id1)%adjlist, varr(id2)%label)
        if (associated(edge)) error stop 'edge 1-2 already in adjlist of 1'
        edge => search_list(varr(id2)%adjlist, varr(id1)%label)
        if (associated(edge)) error stop 'edge 2-1 already in adjlist of 2'
        varr(id1)%adjlist => edge_t(varr(id1)%label, varr(id2)%label, varr(id1)%adjlist, id2)
        varr(id2)%adjlist => edge_t(varr(id2)%label, varr(id1)%label, varr(id2)%adjlist, id1)
      end associate
    end do
print *, 'lines ', size(lines), size(lines)*2
print *, 'vertices ', nv
  end subroutine build_varr


  subroutine free_vertices(this)
    type(vertex_t), intent(inout) :: this(:)

    type(edge_t), pointer :: dele
    integer :: i

    do i=1,size(this)
      do
        if (.not. associated(this(i)%adjlist)) exit
        dele => this(i)%adjlist
        this(i)%adjlist => this(i)%adjlist%next
        deallocate(dele)
        edge_alloc_cnt = edge_alloc_cnt - 1
      end do
    end do
  end subroutine


  function search_list(head, label) result(found)
    type(edge_t), pointer, intent(in) :: head
    character(len=VNAME_LEN), intent(in) :: label
    type(edge_t), pointer :: found

    found => head
    do
      if (.not. associated(found)) exit
      if (found%v2==label) exit
      found => found%next
    end do
  end function search_list


  function count_list(head) result(n)
    type(edge_t), pointer, intent(in) :: head
    integer :: n

    type(edge_t), pointer :: curr

    n = 0
    curr => head
    do
      if (.not. associated(curr)) exit
      n = n + 1
      curr => curr%next
    end do
  end function count_list


  function export_list(head) result(labs)
    type(edge_t), pointer, intent(in) :: head
    character(len=VNAME_LEN), allocatable :: labs(:)

    integer :: i
    type(edge_t), pointer :: curr

    allocate(labs(count_list(head)))
    curr => head
    do i=1, size(labs)
      if (.not. associated(curr)) error stop 'null too early'
      labs(i) = curr%v2
      curr => curr%next
    end do
    if (associated(curr)) error stop 'null not yet'
  end function export_list


  function export_list_ids(head) result(ids)
    type(edge_t), pointer, intent(in) :: head
    integer, allocatable :: ids(:)

    integer :: i
    type(edge_t), pointer :: curr

    allocate(ids(count_list(head)))
    curr => head
    do i=1, size(ids)
      if (.not. associated(curr)) error stop 'null too early'
      ids(i) = curr%idv2
      curr => curr%next
    end do
    if (associated(curr)) error stop 'null not yet'
  end function export_list_ids


  subroutine add_vertex(varr, nv, label, id)
    type(vertex_t), intent(inout), allocatable :: varr(:)
    integer, intent(inout) :: nv
    character(len=VNAME_LEN), intent(in) :: label
    integer, intent(out) :: id

    type(vertex_t), allocatable :: tmp(:)
    integer :: i

    ! make sure, there is a room for a new vertex
    if (.not. allocated(varr)) then
      allocate(varr(8))
      nv = 0
    end if
    if (size(varr)==nv) then
      allocate(tmp(2*nv))
      tmp(1:nv) = varr
      call move_alloc(tmp, varr)
    end if

    ! look, if the vertex is already present
    do i = 1, nv
      if (varr(i)%label==label) exit
    end do

    ! if not, then add it add the end
    if (i==nv+1) then
      nv = nv + 1
      varr(nv)%label=label
      varr(nv)%adjlist => null()
    end if

    ! return the position of new (or old) vertex
    id = i
  end subroutine add_vertex
  

  function new_edge(v1, v2, next, idv2) result(new)
    character(len=VNAME_LEN), intent(in) :: v1, v2
    integer, intent(in) :: idv2
    type(edge_t), pointer, intent(in) :: next
    type(edge_t), pointer :: new

    allocate(new)
    edge_alloc_cnt = edge_alloc_cnt + 1
    new%v1 = v1
    new%v2 = v2
    new%idv2 = idv2
    new%next => next
  end function new_edge

end module day2423_mod