program test_dag
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
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

    subroutine test_dag_add_nodes(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_add_nodes not implemented"
    end subroutine test_dag_add_nodes

    subroutine test_dag_topo_sort(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_topo_sort not implemented"
    end subroutine test_dag_topo_sort

    subroutine test_dag_topo_sort_cycle(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_topo_sort_cycle not implemented"
    end subroutine test_dag_topo_sort_cycle

    subroutine test_dag_reverse_deps(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_reverse_deps not implemented"
    end subroutine test_dag_reverse_deps

    subroutine test_dag_affected_set(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_affected_set not implemented"
    end subroutine test_dag_affected_set

    subroutine test_dag_to_dot(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_to_dot not implemented"
    end subroutine test_dag_to_dot

    subroutine test_dag_levels(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_dag_levels not implemented"
    end subroutine test_dag_levels

end program test_dag
