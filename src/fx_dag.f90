module fx_dag
    implicit none
    private

    integer, parameter, public :: MAX_NODES = 2048

    type, public :: dag_node_t
        character(len=256) :: label = ' '
        integer, allocatable :: edges(:)
        integer :: n_edges = 0
    end type dag_node_t

    type, public :: dag_t
        type(dag_node_t), allocatable :: nodes(:)
        integer :: n_nodes = 0
        integer :: max_nodes = 0
    end type dag_t

    public :: dag_init, dag_add_node, dag_find_node, dag_add_edge
    public :: dag_topo_sort, dag_reverse_deps, dag_affected_set
    public :: dag_to_dot, dag_levels

contains

    subroutine dag_init(d, max_nodes)
        type(dag_t), intent(out) :: d
        integer, intent(in) :: max_nodes
        d%n_nodes = 0
        d%max_nodes = max_nodes
        allocate(d%nodes(max_nodes))
    end subroutine dag_init

    ! Add node with given label. Returns existing id if label already present.
    integer function dag_add_node(d, label)
        type(dag_t), intent(inout) :: d
        character(len=*), intent(in) :: label
        integer :: existing

        existing = dag_find_node(d, label)
        if (existing > 0) then
            dag_add_node = existing
            return
        end if

        if (d%n_nodes >= d%max_nodes) then
            dag_add_node = 0
            return
        end if

        d%n_nodes = d%n_nodes + 1
        d%nodes(d%n_nodes)%label = label
        d%nodes(d%n_nodes)%n_edges = 0
        allocate(d%nodes(d%n_nodes)%edges(4))
        dag_add_node = d%n_nodes
    end function dag_add_node

    ! Linear scan by label. Returns 0 if not found.
    integer function dag_find_node(d, label)
        type(dag_t), intent(in) :: d
        character(len=*), intent(in) :: label
        integer :: i

        dag_find_node = 0
        do i = 1, d%n_nodes
            if (trim(d%nodes(i)%label) == trim(label)) then
                dag_find_node = i
                return
            end if
        end do
    end function dag_find_node

    ! Add edge: from_id depends on to_id (from_id must be built after to_id).
    subroutine dag_add_edge(d, from_id, to_id)
        type(dag_t), intent(inout) :: d
        integer, intent(in) :: from_id
        integer, intent(in) :: to_id
        integer :: n, cap
        integer, allocatable :: tmp(:)

        if (from_id < 1 .or. from_id > d%n_nodes) return
        if (to_id < 1 .or. to_id > d%n_nodes) return
        if (from_id == to_id) return

        ! Check for duplicate edge
        do n = 1, d%nodes(from_id)%n_edges
            if (d%nodes(from_id)%edges(n) == to_id) return
        end do

        n = d%nodes(from_id)%n_edges + 1
        cap = size(d%nodes(from_id)%edges)
        if (n > cap) then
            ! Geometric growth
            allocate(tmp(cap * 2))
            tmp(1:cap) = d%nodes(from_id)%edges
            call move_alloc(tmp, d%nodes(from_id)%edges)
        end if
        d%nodes(from_id)%edges(n) = to_id
        d%nodes(from_id)%n_edges = n
    end subroutine dag_add_edge

    ! Kahn's topological sort. Produces dependencies before dependents.
    ! has_cycle is true if a cycle is detected.
    subroutine dag_topo_sort(d, order, n_order, has_cycle)
        type(dag_t), intent(in) :: d
        integer, intent(out) :: order(:)
        integer, intent(out) :: n_order
        logical, intent(out) :: has_cycle

        integer :: in_deg(MAX_NODES)
        integer :: queue(MAX_NODES)
        integer :: qhead, qtail, node, i, j

        n_order = 0
        has_cycle = .false.
        in_deg = 0

        ! In-degree of node i = number of forward edges leaving i (its dependency count)
        do i = 1, d%n_nodes
            in_deg(i) = d%nodes(i)%n_edges
        end do

        ! Seed queue with dependency-free nodes
        qhead = 1
        qtail = 0
        do i = 1, d%n_nodes
            if (in_deg(i) == 0) then
                qtail = qtail + 1
                queue(qtail) = i
            end if
        end do

        do while (qhead <= qtail)
            node = queue(qhead)
            qhead = qhead + 1
            n_order = n_order + 1
            if (n_order <= size(order)) order(n_order) = node

            ! For all nodes that depend on 'node', satisfy one dependency
            do i = 1, d%n_nodes
                do j = 1, d%nodes(i)%n_edges
                    if (d%nodes(i)%edges(j) == node) then
                        in_deg(i) = in_deg(i) - 1
                        if (in_deg(i) == 0) then
                            qtail = qtail + 1
                            queue(qtail) = i
                        end if
                    end if
                end do
            end do
        end do

        has_cycle = (n_order /= d%n_nodes)
    end subroutine dag_topo_sort

    ! BFS to find all nodes transitively depending on node_id (inclusive).
    subroutine dag_reverse_deps(d, node_id, affected, n_affected)
        type(dag_t), intent(in) :: d
        integer, intent(in) :: node_id
        integer, intent(out) :: affected(:)
        integer, intent(out) :: n_affected

        logical :: visited(MAX_NODES)
        integer :: queue(MAX_NODES)
        integer :: qhead, qtail, current, i, j

        n_affected = 0
        visited = .false.

        if (node_id < 1 .or. node_id > d%n_nodes) return

        visited(node_id) = .true.
        qhead = 1
        qtail = 1
        queue(1) = node_id

        do while (qhead <= qtail)
            current = queue(qhead)
            qhead = qhead + 1
            n_affected = n_affected + 1
            if (n_affected <= size(affected)) affected(n_affected) = current

            ! Find nodes that have current in their dependency list
            do i = 1, d%n_nodes
                if (visited(i)) cycle
                do j = 1, d%nodes(i)%n_edges
                    if (d%nodes(i)%edges(j) == current) then
                        visited(i) = .true.
                        qtail = qtail + 1
                        queue(qtail) = i
                        exit
                    end if
                end do
            end do
        end do
    end subroutine dag_reverse_deps

    ! Union of reverse_deps over all changed nodes, deduplicated.
    subroutine dag_affected_set(d, changed_ids, n_changed, &
            affected, n_affected)
        type(dag_t), intent(in) :: d
        integer, intent(in) :: n_changed
        integer, intent(in) :: changed_ids(n_changed)
        integer, intent(out) :: affected(:)
        integer, intent(out) :: n_affected

        logical :: visited(MAX_NODES)
        integer :: queue(MAX_NODES)
        integer :: qhead, qtail, current, i, j, k

        n_affected = 0
        visited = .false.

        ! Seed queue with all changed nodes
        qhead = 1
        qtail = 0
        do k = 1, n_changed
            i = changed_ids(k)
            if (i < 1 .or. i > d%n_nodes) cycle
            if (.not. visited(i)) then
                visited(i) = .true.
                qtail = qtail + 1
                queue(qtail) = i
            end if
        end do

        do while (qhead <= qtail)
            current = queue(qhead)
            qhead = qhead + 1
            n_affected = n_affected + 1
            if (n_affected <= size(affected)) affected(n_affected) = current

            do i = 1, d%n_nodes
                if (visited(i)) cycle
                do j = 1, d%nodes(i)%n_edges
                    if (d%nodes(i)%edges(j) == current) then
                        visited(i) = .true.
                        qtail = qtail + 1
                        queue(qtail) = i
                        exit
                    end if
                end do
            end do
        end do
    end subroutine dag_affected_set

    ! Emit graphviz dot representation.
    subroutine dag_to_dot(d, output)
        type(dag_t), intent(in) :: d
        character(len=:), allocatable, intent(out) :: output
        integer :: i, j

        output = 'digraph {' // achar(10) // &
            'rankdir=BT;' // achar(10) // &
            'node[shape=box];' // achar(10)

        do i = 1, d%n_nodes
            do j = 1, d%nodes(i)%n_edges
                output = output // '"' // trim(d%nodes(i)%label) // '"' // &
                    ' -> "' // &
                    trim(d%nodes(d%nodes(i)%edges(j))%label) // '"' // &
                    ';' // achar(10)
            end do
        end do

        output = output // '}'
    end subroutine dag_to_dot

    ! Assign level to each node in topo order.
    ! Level 0 = no dependencies. Level k = all deps at levels < k.
    ! !$omp parallel do
    subroutine dag_levels(d, order, n_order, levels, n_levels)
        type(dag_t), intent(in) :: d
        integer, intent(in) :: n_order
        integer, intent(in) :: order(n_order)
        integer, intent(out) :: levels(:)
        integer, intent(out) :: n_levels

        integer :: node_level(MAX_NODES)
        integer :: i, j, node, dep, lv

        node_level = 0
        n_levels = 0
        levels = 0

        do i = 1, n_order
            node = order(i)
            lv = 0
            do j = 1, d%nodes(node)%n_edges
                dep = d%nodes(node)%edges(j)
                if (node_level(dep) + 1 > lv) lv = node_level(dep) + 1
            end do
            node_level(node) = lv
            if (lv + 1 > n_levels) n_levels = lv + 1
            if (i <= size(levels)) levels(i) = lv
        end do
    end subroutine dag_levels

end module fx_dag
