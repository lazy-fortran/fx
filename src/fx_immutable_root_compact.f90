module fx_immutable_root_compact
    !! Bounded conversion of old Fo blob roots into one verified Fx tree root.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
    use fx_hash, only: sha256_string
    use fx_immutable_manifest, only: immutable_tree_entry_t, &
        immutable_entries_canonical, immutable_manifest_encode, IMMUTABLE_BLOB
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_store_publication_lease_acquire, &
        immutable_store_lease_release, IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_INVALID
    use fx_immutable_tree, only: immutable_store_put_tree
    implicit none
    private

    integer, parameter :: HASH_LEN = 64
    public :: immutable_store_compact_generation_roots

    interface
        integer(c_int) function c_candidate(root, cursor, max_rows, &
                max_children, start, start_cap, reason, reason_cap, ids, &
                ids_cap, count) &
                bind(C, name='fx_immutable_lease_generation_candidate')
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(inout) :: cursor
            integer(c_int), value :: max_rows, max_children
            character(kind=c_char), intent(out) :: start(*), reason(*), ids(*)
            integer(c_size_t), value :: start_cap, reason_cap, ids_cap
            integer(c_int), intent(out) :: count
        end function c_candidate

        integer(c_int) function c_replace(root, start, reason, ids, count, &
                tree_id, publication_start, publication_token) &
                bind(C, name='fx_immutable_lease_generation_replace')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), start(*), reason(*)
            character(kind=c_char), intent(in) :: ids(*), tree_id(*)
            integer(c_int), value :: count
            character(kind=c_char), intent(in) :: publication_start(*)
            character(kind=c_char), intent(in) :: publication_token(*)
        end function c_replace
    end interface

contains

    subroutine immutable_store_compact_generation_roots(store, max_scan_rows, &
            max_groups, max_children, scanned_rows, compacted, ierr, scan_cursor)
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: max_scan_rows, max_groups, max_children
        integer, intent(out) :: scanned_rows, compacted, ierr
        integer, intent(inout), optional :: scan_cursor
        type(immutable_tree_entry_t), allocatable :: entries(:), canonical(:)
        type(immutable_lease_t) :: publication
        character(kind=c_char, len=256) :: start, reason
        character(kind=c_char, len=:), allocatable :: ids_text
        character(len=HASH_LEN), allocatable :: ids(:)
        character(len=HASH_LEN) :: tree_id, published_id
        character(len=4), allocatable :: kinds(:)
        character(len=:), allocatable :: manifest
        integer(c_int) :: cursor, count, result
        integer :: i, status, release_status, before_cursor

        scanned_rows = 0
        compacted = 0
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized .or. max_scan_rows < 1 .or. &
            max_groups < 1 .or. max_children < 2 .or. max_children > 4096) return
        allocate (character(kind=c_char, len=max_children * 65 + 1) :: ids_text)
        cursor = 0_c_int
        if (present(scan_cursor)) cursor = int(max(0, scan_cursor), c_int)
        do while (scanned_rows < max_scan_rows .and. compacted < max_groups)
            before_cursor = int(cursor)
            result = c_candidate(store%root_dir//c_null_char, cursor, &
                int(max_scan_rows - scanned_rows, c_int), &
                int(max_children, c_int), start, int(len(start), c_size_t), &
                reason, int(len(reason), c_size_t), ids_text, &
                int(len(ids_text), c_size_t), count)
            scanned_rows = scanned_rows + int(cursor) - before_cursor
            if (present(scan_cursor)) scan_cursor = int(cursor)
            if (result < 0_c_int) then
                ierr = IMMUTABLE_IO_ERROR
                return
            end if
            if (result /= 0_c_int) then
                if (present(scan_cursor)) then
                    if (int(cursor) == before_cursor) scan_cursor = 0
                end if
                exit
            end if
            allocate (entries(count), ids(count + 1), kinds(count + 1))
            do i = 1, count
                ids(i) = ids_text((i - 1) * 65 + 1:(i - 1) * 65 + HASH_LEN)
                entries(i)%path = ids(i)
                entries(i)%role = 'generation'
                entries(i)%object_id = ids(i)
                entries(i)%mode = 420
                entries(i)%kind = IMMUTABLE_BLOB
            end do
            call immutable_entries_canonical(entries, canonical, status)
            if (status /= IMMUTABLE_OK) then
                deallocate (entries, ids, kinds)
                cycle
            end if
            call immutable_manifest_encode(canonical, manifest)
            tree_id = sha256_string(manifest)
            ids(count + 1) = tree_id
            kinds(:count) = 'blob'
            kinds(count + 1) = 'tree'
            call immutable_store_publication_lease_acquire(store, &
                'fx-root-compact', store%writer_start, 'generation-migration', &
                kinds, ids, publication, status)
            if (status /= IMMUTABLE_OK) then
                ierr = status
                return
            end if
            call immutable_store_put_tree(store, canonical, published_id, status)
            if (status == IMMUTABLE_OK .and. published_id /= tree_id) &
                status = IMMUTABLE_INVALID
            if (status == IMMUTABLE_OK) then
                result = c_replace(store%root_dir//c_null_char, &
                    start(:index(start, c_null_char) - 1)//c_null_char, &
                    reason(:index(reason, c_null_char) - 1)//c_null_char, &
                    ids_text, count, tree_id//c_null_char, &
                    store%writer_start//c_null_char, &
                    publication%token//c_null_char)
                if (result == 0_c_int) compacted = compacted + 1
                if (result < 0_c_int) status = IMMUTABLE_IO_ERROR
            end if
            call immutable_store_lease_release(store, publication, release_status)
            if (release_status /= IMMUTABLE_OK) status = release_status
            deallocate (entries, canonical, ids, kinds)
            if (status /= IMMUTABLE_OK) then
                ierr = status
                return
            end if
        end do
        ierr = IMMUTABLE_OK
    end subroutine immutable_store_compact_generation_roots

end module fx_immutable_root_compact
