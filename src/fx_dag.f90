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
        error stop "fx_dag:dag_init not implemented"
    end subroutine dag_init

    integer function dag_add_node(d, label)
        type(dag_t), intent(inout) :: d
        character(len=*), intent(in) :: label
        error stop "fx_dag:dag_add_node not implemented"
    end function dag_add_node

    integer function dag_find_node(d, label)
        type(dag_t), intent(in) :: d
        character(len=*), intent(in) :: label
        error stop "fx_dag:dag_find_node not implemented"
    end function dag_find_node

    subroutine dag_add_edge(d, from_id, to_id)
        type(dag_t), intent(inout) :: d
        integer, intent(in) :: from_id
        integer, intent(in) :: to_id
        error stop "fx_dag:dag_add_edge not implemented"
    end subroutine dag_add_edge

    subroutine dag_topo_sort(d, order, n_order, has_cycle)
        type(dag_t), intent(in) :: d
        integer, intent(out) :: order(:)
        integer, intent(out) :: n_order
        logical, intent(out) :: has_cycle
        error stop "fx_dag:dag_topo_sort not implemented"
    end subroutine dag_topo_sort

    subroutine dag_reverse_deps(d, node_id, affected, n_affected)
        type(dag_t), intent(in) :: d
        integer, intent(in) :: node_id
        integer, intent(out) :: affected(:)
        integer, intent(out) :: n_affected
        error stop "fx_dag:dag_reverse_deps not implemented"
    end subroutine dag_reverse_deps

    subroutine dag_affected_set(d, changed_ids, n_changed, &
            affected, n_affected)
        type(dag_t), intent(in) :: d
        integer, intent(in) :: n_changed
        integer, intent(in) :: changed_ids(n_changed)
        integer, intent(out) :: affected(:)
        integer, intent(out) :: n_affected
        error stop "fx_dag:dag_affected_set not implemented"
    end subroutine dag_affected_set

    subroutine dag_to_dot(d, output)
        type(dag_t), intent(in) :: d
        character(len=:), allocatable, intent(out) :: output
        error stop "fx_dag:dag_to_dot not implemented"
    end subroutine dag_to_dot

    subroutine dag_levels(d, order, n_order, levels, n_levels)
        ! Nodes at the same level have no inter-dependencies and can
        ! execute concurrently.
        !$omp parallel do
        type(dag_t), intent(in) :: d
        integer, intent(in) :: n_order
        integer, intent(in) :: order(n_order)
        integer, intent(out) :: levels(:)
        integer, intent(out) :: n_levels
        error stop "fx_dag:dag_levels not implemented"
    end subroutine dag_levels

end module fx_dag
