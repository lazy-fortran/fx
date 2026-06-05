program test_dag
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit, &
                       test_assert, test_assert_equal_int
    use fx_dag, only: dag_t, dag_node_t, dag_init, dag_add_node, &
                      dag_find_node, dag_add_edge, dag_topo_sort, &
                      dag_reverse_deps, dag_affected_set, dag_to_dot, &
                      dag_levels, MAX_NODES
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_dag')
    call test_dag_add_nodes(suite)
    call test_dag_topo_sort(suite)
    call test_dag_topo_sort_cycle(suite)
    call test_dag_reverse_deps(suite)
    call test_dag_affected_set(suite)
    call test_dag_to_dot(suite)
    call test_dag_levels(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    ! Return true if value v is in array arr(1:n).
    logical function contains_id(arr, n, v)
        integer, intent(in) :: arr(:), n, v
        integer :: i
        contains_id = .false.
        do i = 1, n
            if (arr(i) == v) then
                contains_id = .true.
                return
            end if
        end do
    end function contains_id

    ! Return the position of node with id v in order(1:n), or 0.
    integer function pos_of(order, n, v)
        integer, intent(in) :: order(:), n, v
        integer :: i
        pos_of = 0
        do i = 1, n
            if (order(i) == v) then
                pos_of = i
                return
            end if
        end do
    end function pos_of

    subroutine test_dag_add_nodes(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c, id_dup

        call dag_init(d, 10)
        call test_assert_equal_int(suite, 0, d%n_nodes, 'init: n_nodes=0')
        call test_assert_equal_int(suite, 10, d%max_nodes, 'init: max_nodes=10')

        id_a = dag_add_node(d, 'a')
        call test_assert_equal_int(suite, 1, id_a, 'add_node: first id=1')
        call test_assert_equal_int(suite, 1, d%n_nodes, 'add_node: n_nodes=1')

        id_b = dag_add_node(d, 'b')
        call test_assert_equal_int(suite, 2, id_b, 'add_node: second id=2')

        id_c = dag_add_node(d, 'c')
        call test_assert_equal_int(suite, 3, id_c, 'add_node: third id=3')

        ! Duplicate label returns existing id
        id_dup = dag_add_node(d, 'a')
        call test_assert_equal_int(suite, 1, id_dup, 'add_node: duplicate returns 1')
        call test_assert_equal_int(suite, 3, d%n_nodes, 'add_node: n_nodes unchanged after dup')

        ! find_node
        call test_assert_equal_int(suite, 2, dag_find_node(d, 'b'), 'find_node: b=2')
        call test_assert_equal_int(suite, 0, dag_find_node(d, 'z'), 'find_node: missing=0')
    end subroutine test_dag_add_nodes

    subroutine test_dag_topo_sort(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c
        integer :: order(MAX_NODES), n_order
        logical :: has_cycle

        ! a <- b <- c  (c depends on b, b depends on a)
        call dag_init(d, 16)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        id_c = dag_add_node(d, 'c')
        call dag_add_edge(d, id_b, id_a)   ! b depends on a
        call dag_add_edge(d, id_c, id_b)   ! c depends on b
        call dag_add_edge(d, id_c, id_a)   ! c also depends on a directly

        call dag_topo_sort(d, order, n_order, has_cycle)
        call test_assert_equal_int(suite, 3, n_order, 'topo_sort: n_order=3')
        call test_assert(suite, .not. has_cycle, 'topo_sort: no cycle')

        ! a must come before b and c; b must come before c
        call test_assert(suite, pos_of(order, n_order, id_a) < pos_of(order, n_order, id_b), &
                         'topo_sort: a before b')
        call test_assert(suite, pos_of(order, n_order, id_a) < pos_of(order, n_order, id_c), &
                         'topo_sort: a before c')
        call test_assert(suite, pos_of(order, n_order, id_b) < pos_of(order, n_order, id_c), &
                         'topo_sort: b before c')

        ! Single node: no edges
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'x')
        call dag_topo_sort(d, order, n_order, has_cycle)
        call test_assert_equal_int(suite, 1, n_order, 'topo_sort single: n_order=1')
        call test_assert(suite, .not. has_cycle, 'topo_sort single: no cycle')
        call test_assert_equal_int(suite, id_a, order(1), 'topo_sort single: order[1]=x')

        ! Duplicate edge must not affect result
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'p')
        id_b = dag_add_node(d, 'q')
        call dag_add_edge(d, id_b, id_a)
        call dag_add_edge(d, id_b, id_a)   ! duplicate: should be ignored
        call dag_topo_sort(d, order, n_order, has_cycle)
        call test_assert_equal_int(suite, 2, n_order, 'topo_sort dup edge: n_order=2')
        call test_assert(suite, .not. has_cycle, 'topo_sort dup edge: no cycle')
    end subroutine test_dag_topo_sort

    subroutine test_dag_topo_sort_cycle(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c
        integer :: order(MAX_NODES), n_order
        logical :: has_cycle

        ! Direct cycle: a <-> b
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        call dag_add_edge(d, id_a, id_b)
        call dag_add_edge(d, id_b, id_a)
        call dag_topo_sort(d, order, n_order, has_cycle)
        call test_assert(suite, has_cycle, 'topo_sort_cycle: direct cycle detected')

        ! Triangular cycle: a -> b -> c -> a
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        id_c = dag_add_node(d, 'c')
        call dag_add_edge(d, id_a, id_b)
        call dag_add_edge(d, id_b, id_c)
        call dag_add_edge(d, id_c, id_a)
        call dag_topo_sort(d, order, n_order, has_cycle)
        call test_assert(suite, has_cycle, 'topo_sort_cycle: 3-cycle detected')
        call test_assert(suite, n_order < 3, 'topo_sort_cycle: partial order only')
    end subroutine test_dag_topo_sort_cycle

    subroutine test_dag_reverse_deps(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c, id_d
        integer :: affected(MAX_NODES), n_affected

        ! b depends on a, c depends on b, d is independent
        call dag_init(d, 8)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        id_c = dag_add_node(d, 'c')
        id_d = dag_add_node(d, 'd')
        call dag_add_edge(d, id_b, id_a)
        call dag_add_edge(d, id_c, id_b)

        ! reverse deps of a: a itself, b, c (not d)
        call dag_reverse_deps(d, id_a, affected, n_affected)
        call test_assert_equal_int(suite, 3, n_affected, 'rdeps(a): 3 nodes')
        call test_assert(suite, contains_id(affected, n_affected, id_a), 'rdeps(a): includes a')
        call test_assert(suite, contains_id(affected, n_affected, id_b), 'rdeps(a): includes b')
        call test_assert(suite, contains_id(affected, n_affected, id_c), 'rdeps(a): includes c')
        call test_assert(suite, .not. contains_id(affected, n_affected, id_d), 'rdeps(a): excludes d')

        ! reverse deps of b: b, c
        call dag_reverse_deps(d, id_b, affected, n_affected)
        call test_assert_equal_int(suite, 2, n_affected, 'rdeps(b): 2 nodes')
        call test_assert(suite, contains_id(affected, n_affected, id_b), 'rdeps(b): includes b')
        call test_assert(suite, contains_id(affected, n_affected, id_c), 'rdeps(b): includes c')

        ! reverse deps of c (leaf in reverse): only c
        call dag_reverse_deps(d, id_c, affected, n_affected)
        call test_assert_equal_int(suite, 1, n_affected, 'rdeps(c): 1 node')
        call test_assert(suite, contains_id(affected, n_affected, id_c), 'rdeps(c): includes c')

        ! invalid id returns empty
        call dag_reverse_deps(d, 0, affected, n_affected)
        call test_assert_equal_int(suite, 0, n_affected, 'rdeps(0): empty')
    end subroutine test_dag_reverse_deps

    subroutine test_dag_affected_set(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c, id_d
        integer :: changed(4), affected(MAX_NODES), n_affected

        ! b depends on a, c depends on b, d depends on a (diamond without direct c-d)
        call dag_init(d, 8)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        id_c = dag_add_node(d, 'c')
        id_d = dag_add_node(d, 'd')
        call dag_add_edge(d, id_b, id_a)
        call dag_add_edge(d, id_c, id_b)
        call dag_add_edge(d, id_d, id_a)

        ! changed = {a}: all of a, b, c, d affected
        changed(1) = id_a
        call dag_affected_set(d, changed, 1, affected, n_affected)
        call test_assert_equal_int(suite, 4, n_affected, 'affected({a}): 4 nodes')
        call test_assert(suite, contains_id(affected, n_affected, id_a), 'affected({a}): a')
        call test_assert(suite, contains_id(affected, n_affected, id_b), 'affected({a}): b')
        call test_assert(suite, contains_id(affected, n_affected, id_c), 'affected({a}): c')
        call test_assert(suite, contains_id(affected, n_affected, id_d), 'affected({a}): d')

        ! changed = {b, d}: b, c, d affected (not a)
        changed(1) = id_b
        changed(2) = id_d
        call dag_affected_set(d, changed, 2, affected, n_affected)
        call test_assert_equal_int(suite, 3, n_affected, 'affected({b,d}): 3 nodes')
        call test_assert(suite, .not. contains_id(affected, n_affected, id_a), 'affected({b,d}): no a')
        call test_assert(suite, contains_id(affected, n_affected, id_b), 'affected({b,d}): b')
        call test_assert(suite, contains_id(affected, n_affected, id_c), 'affected({b,d}): c')
        call test_assert(suite, contains_id(affected, n_affected, id_d), 'affected({b,d}): d')

        ! Empty changed set: 0 affected
        call dag_affected_set(d, changed, 0, affected, n_affected)
        call test_assert_equal_int(suite, 0, n_affected, 'affected({}): 0 nodes')
    end subroutine test_dag_affected_set

    subroutine test_dag_to_dot(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b
        character(len=:), allocatable :: dot

        ! Single edge: b depends on a
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        call dag_add_edge(d, id_b, id_a)

        call dag_to_dot(d, dot)
        call test_assert(suite, index(dot, 'digraph {') > 0, 'to_dot: digraph header')
        call test_assert(suite, index(dot, 'rankdir=BT') > 0, 'to_dot: rankdir=BT')
        call test_assert(suite, index(dot, 'node[shape=box]') > 0, 'to_dot: node style')
        call test_assert(suite, index(dot, '"b" -> "a"') > 0, 'to_dot: edge b->a')
        call test_assert(suite, dot(len(dot):len(dot)) == '}', 'to_dot: closing brace')

        ! Empty graph: no edges
        call dag_init(d, 4)
        id_a = dag_add_node(d, 'x')
        call dag_to_dot(d, dot)
        call test_assert(suite, index(dot, 'digraph {') > 0, 'to_dot empty: header')
        call test_assert(suite, index(dot, '->') == 0, 'to_dot empty: no edges')
    end subroutine test_dag_to_dot

    subroutine test_dag_levels(suite)
        type(test_suite_t), intent(inout) :: suite
        type(dag_t) :: d
        integer :: id_a, id_b, id_c
        integer :: order(MAX_NODES), n_order
        integer :: levels(MAX_NODES), n_levels
        logical :: has_cycle
        integer :: pa, pb, pc

        ! chain: a <- b <- c
        call dag_init(d, 8)
        id_a = dag_add_node(d, 'a')
        id_b = dag_add_node(d, 'b')
        id_c = dag_add_node(d, 'c')
        call dag_add_edge(d, id_b, id_a)
        call dag_add_edge(d, id_c, id_b)

        call dag_topo_sort(d, order, n_order, has_cycle)
        call dag_levels(d, order, n_order, levels, n_levels)

        call test_assert_equal_int(suite, 3, n_levels, 'levels chain: n_levels=3')
        call test_assert_equal_int(suite, 3, n_order, 'levels chain: n_order=3')

        ! Look up level of each node by its position in order
        pa = pos_of(order, n_order, id_a)
        pb = pos_of(order, n_order, id_b)
        pc = pos_of(order, n_order, id_c)
        call test_assert_equal_int(suite, 0, levels(pa), 'levels chain: level(a)=0')
        call test_assert_equal_int(suite, 1, levels(pb), 'levels chain: level(b)=1')
        call test_assert_equal_int(suite, 2, levels(pc), 'levels chain: level(c)=2')

        ! Two independent roots: both at level 0
        call dag_init(d, 8)
        id_a = dag_add_node(d, 'r1')
        id_b = dag_add_node(d, 'r2')
        id_c = dag_add_node(d, 'top')
        call dag_add_edge(d, id_c, id_a)
        call dag_add_edge(d, id_c, id_b)

        call dag_topo_sort(d, order, n_order, has_cycle)
        call dag_levels(d, order, n_order, levels, n_levels)

        call test_assert_equal_int(suite, 2, n_levels, 'levels diamond: n_levels=2')
        pa = pos_of(order, n_order, id_a)
        pb = pos_of(order, n_order, id_b)
        pc = pos_of(order, n_order, id_c)
        call test_assert_equal_int(suite, 0, levels(pa), 'levels diamond: level(r1)=0')
        call test_assert_equal_int(suite, 0, levels(pb), 'levels diamond: level(r2)=0')
        call test_assert_equal_int(suite, 1, levels(pc), 'levels diamond: level(top)=1')
    end subroutine test_dag_levels

end program test_dag
