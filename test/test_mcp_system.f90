program test_mcp_system
    use iso_c_binding, only: c_int,c_char,c_ptr,c_null_ptr,c_null_char,c_associated
    use mcp_test_json, only: document_t,parse_json,child,string_is,atom_is,array_size
    use fx_mcp_test_os, only: fx_test_spawn_argv, fx_test_is_executable, &
        fx_test_write, fx_test_read, fx_test_close
    implicit none
    type :: session_t
        type(c_ptr) :: handle=c_null_ptr
        character(len=:), allocatable :: pending
        logical :: io_failed=.false.
    end type
    integer :: failures
    character(len=4096) :: server, requested
    logical :: server_found
    failures=0
    requested=' '
    call get_command_argument(1,requested)
    if(len_trim(requested)==0) call get_environment_variable('FX_MCP_SERVER',requested)
    call find_server(trim(requested),server,server_found)
    if(.not.server_found) then
        write(*,'(A)') 'FAIL: exact sibling fx-mcp-server is missing or not executable'
        stop 1
    end if
    write(*,'(A)') 'fx MCP independent Fortran process oracle'
    call run_mode(trim(server),.true.)
    call run_mode(trim(server),.false.)
    write(*,'(A,I0,A)') 'MCP process oracle failures: ',failures
    if(failures/=0) stop 1
contains
    subroutine check(ok,label)
        logical,intent(in)::ok
        character(len=*),intent(in)::label
        if(ok) then
            write(*,'(A)') '  ok: '//label
        else
            failures=failures+1
            write(*,'(A)') '  FAIL: '//label
        end if
    end subroutine

    subroutine find_server(requested,path,found)
        character(len=*),intent(in)::requested
        character(len=*),intent(out)::path
        logical,intent(out)::found
        character(len=4096)::test_binary
        character(len=4096)::sibling
        integer::n,slash,dir_length
        path=' '; sibling=' '; found=.false.
        test_binary=' '
        call get_command_argument(0,test_binary,length=n)
        if(n<=0.or.n>len(test_binary)) return
        slash=scan(trim(test_binary), '/', back=.true.)
        if(slash<=1) return
        dir_length=slash-1
        if(ends_with(test_binary(:dir_length),'/build/fo/bin')) then
            sibling=test_binary(:dir_length)//'/fx-mcp-server'
        else if(ends_with(test_binary(:dir_length),'/test')) then
            sibling=test_binary(:dir_length-5)//'/app/fx-mcp-server'
        else
            sibling=test_binary(:dir_length)//'/fx-mcp-server'
        end if
        if(len_trim(requested)>0) then
            path=requested
        else
            path=sibling
        end if
        found=fx_test_is_executable(trim(path)//c_null_char)==1
    end subroutine

    logical function ends_with(text,suffix)
        character(len=*),intent(in)::text,suffix
        integer::text_length,suffix_length
        text_length=len_trim(text); suffix_length=len(suffix)
        ends_with=.false.
        if(text_length<suffix_length) return
        ends_with=text(text_length-suffix_length+1:text_length)==suffix
    end function ends_with

    subroutine run_mode(path,framed)
        character(len=*),intent(in)::path
        logical,intent(in)::framed
        type(session_t)::s
        type(document_t)::d
        character(len=:),allocatable::response,request
        character(kind=c_char),allocatable::arguments(:)
        integer::position,i
        integer::root,result,tools,error,code,bytes
        character(len=32)::label
        allocate(arguments(len_trim(path)+1))
        position=1
        do i=1,len_trim(path)
            arguments(position)=path(i:i); position=position+1
        end do
        arguments(position)=c_null_char
        s%handle=fx_test_spawn_argv(arguments,1_c_int,0_c_int)
        call check(c_associated(s%handle),'server process starts')
        if(.not.c_associated(s%handle)) return
        if(framed) then; label='Content-Length'; else; label='bare JSON'; end if
        write(*,'(A)') '--- '//trim(label)//' ---'

        request='{"jsonrpc":"2.0","id":1,"method":"initialize","params":'// &
            '{"protocolVersion":"2025-03-26","capabilities":{},'// &
            '"clientInfo":{"name":"test","version":"1.0"}}}'
        call send_request(s,request,framed)
        call receive_response(s,framed,response)
        call parse_json(response,d); root=1
        call check(d%valid,'initialize response is valid JSON')
        call check(string_is(d,child(d,root,'jsonrpc'),'2.0'),'initialize JSON-RPC version')
        call check(atom_is(d,child(d,root,'id'),'1'),'initialize response ID')
        result=child(d,root,'result')
        call check(string_is(d,child(d,result,'protocolVersion'),'2025-03-26'),'initialize echoes protocol version')
        block
            integer :: info
            info=child(d,result,'serverInfo')
            call check(string_is(d,child(d,info,'name'),'fx'),'initialize identifies fx server')
            call check(string_is(d,child(d,info,'version'),'0.1.0'),'initialize reports server version')
        end block

        call send_request(s,'{"jsonrpc":"2.0","id":2,"method":"ping"}',framed)
        call receive_response(s,framed,response); call parse_json(response,d)
        call check(atom_is(d,child(d,1,'id'),'2').and.child(d,1,'result')>0,'ping returns matching result')

        call send_request(s,'{"jsonrpc":"2.0","method":"notifications/initialized"}',framed)
        call send_request(s,'{"jsonrpc":"2.0","id":3,"method":"tools/list"}',framed)
        call receive_response(s,framed,response); call parse_json(response,d)
        result=child(d,1,'result'); tools=child(d,result,'tools')
        call check(atom_is(d,child(d,1,'id'),'3'),'notification has no response ahead of tools/list')
        call check(array_size(d,tools)>0,'tools/list contains at least one tool')

        call send_request(s,'{"jsonrpc":"2.0","method":"tools/call","params":{"name":"fx","arguments":{"action":"check"}}}',framed)
        call send_request(s,'{"jsonrpc":"2.0","id":4,"method":"tools/call",'// &
            '"params":{"name":"fx","arguments":{"action":"status"}}}',framed)
        call receive_response(s,framed,response); call parse_json(response,d)
        call check(atom_is(d,child(d,1,'id'),'4').and.child(d,1,'result')>0, &
            'tools/call notification is silent and request ID is retained')

        request=boundary_request(32768,7,bytes)
        call check(bytes==32768,'boundary request is exactly 32 KiB')
        call send_request(s,request,framed); call receive_response(s,framed,response)
        call parse_json(response,d)
        call check(atom_is(d,child(d,1,'id'),'7'),'exact 32 KiB request is accepted')

        request=boundary_request(33000,8,bytes)
        call check(bytes>32768,'oversize request exceeds 32 KiB')
        call send_request(s,request,framed); call receive_response(s,framed,response)
        call parse_json(response,d); error=child(d,1,'error')
        call check(atom_is(d,child(d,error,'code'),'-32700'),'oversize request returns parse error')
        call send_request(s,'{"jsonrpc":"2.0","id":9,"method":"tools/list"}',framed)
        call receive_response(s,framed,response); call parse_json(response,d)
        call check(atom_is(d,child(d,1,'id'),'9').and.child(d,child(d,1,'result'),'tools')>0,'server recovers after oversize input')

        call send_request(s,'{"jsonrpc":"2.0","id":10,"method":"bogus/method"}',framed)
        call receive_response(s,framed,response); call parse_json(response,d); error=child(d,1,'error')
        call check(atom_is(d,child(d,error,'code'),'-32601'),'unknown method uses method-not-found')
        if(framed) then
            call send_raw(s,'Content-Length: 2'//achar(13)//achar(10)// &
                achar(13)//achar(10)//'}{')
        else
            call send_raw(s,'{"jsonrpc":"2.0","id":13,"method":"broken'//achar(10))
        end if
        call receive_response(s,framed,response); call parse_json(response,d); error=child(d,1,'error')
        call check(atom_is(d,child(d,error,'code'),'-32700'),'malformed JSON returns parse error')
        if(framed) then
            call send_raw(s,'Content-Length: abc'//achar(13)//achar(10)// &
                achar(13)//achar(10))
            call receive_response(s,framed,response); call parse_json(response,d); error=child(d,1,'error')
            call check(atom_is(d,child(d,error,'code'),'-32700'),'malformed length header returns parse error')
            call send_request(s,'{"jsonrpc":"2.0","id":12,"method":"ping"}',framed)
            call receive_response(s,framed,response); call parse_json(response,d)
            call check(child(d,child(d,1,'result'),'x')==0.and.child(d,1,'result')>0,'server recovers after malformed header')
        end if
        call send_request(s,'{"jsonrpc":"2.0","id":11,"method":"shutdown"}',framed)
        call receive_response(s,framed,response); call parse_json(response,d)
        call check(atom_is(d,child(d,1,'id'),'11').and.atom_is(d,child(d,1,'result'),'null'),'shutdown responds')
        code=fx_test_close(s%handle,3000); s%handle=c_null_ptr
        call check(code==0,'server exits cleanly within bounded shutdown')
    end subroutine

    function boundary_request(target,id,nbytes) result(out)
        integer,intent(in)::target,id
        integer,intent(out)::nbytes
        character(len=:),allocatable::out,pre,suf
        character(len=12)::idtxt
        write(idtxt,'(I0)') id
        pre='{"jsonrpc":"2.0","id":'//trim(idtxt)//',"method":"tools/call",'// &
            '"params":{"name":"fx","arguments":{"action":"status","payload":"'
        suf='"}}}'
        out=pre//repeat('x',max(0,target-len(pre)-len(suf)))//suf
        nbytes=len(out)
    end function

    subroutine send_request(s,body,framed)
        type(session_t),intent(inout)::s
        character(len=*),intent(in)::body
        logical,intent(in)::framed
        character(len=32)::num
        if(framed) then
            write(num,'(I0)') len(body)
            call send_raw(s,'Content-Length: '//trim(num)//achar(13)//achar(10)// &
                achar(13)//achar(10)//body)
        else
            call send_raw(s,body//achar(10))
        end if
    end subroutine

    subroutine send_raw(s,bytes)
        type(session_t),intent(inout)::s
        character(len=*),intent(in)::bytes
        integer(c_int)::n
        n=fx_test_write(s%handle,bytes,int(len(bytes),c_int))
        call check(n==len(bytes),'request bytes written')
    end subroutine

    subroutine receive_response(s,framed,response)
        type(session_t),intent(inout)::s
        logical,intent(in)::framed
        character(len=:),allocatable,intent(out)::response
        character(len=:),allocatable::header
        integer::p,n,need,ios
        if(framed) then
            call take_until(s,achar(13)//achar(10)//achar(13)//achar(10),header)
            p=index(header,'Content-Length:')
            if(p==0) then; call check(.false.,'response has Content-Length header'); response='{}'; return; end if
            read(header(p+15:),*,iostat=ios) need
            if(ios/=0.or.need<0.or.need>100000) then; response='{}'; call check(.false.,'response length parses'); return; end if
            call take_exact(s,need,response)
        else
            call take_until(s,achar(10),response)
        end if
    end subroutine

    subroutine take_until(s,mark,out)
        type(session_t),intent(inout)::s
        character(len=*),intent(in)::mark
        character(len=:),allocatable,intent(out)::out
        integer::p
        do
            p=index(s%pending,mark)
            if(p>0) then
                out=s%pending(:p-1); s%pending=s%pending(p+len(mark):); return
            end if
            call more(s)
            if(s%io_failed.or.len(s%pending)>200000) exit
        end do
        out=''; call check(.false.,'response arrives before timeout')
    end subroutine

    subroutine take_exact(s,n,out)
        type(session_t),intent(inout)::s
        integer,intent(in)::n
        character(len=:),allocatable,intent(out)::out
        do while(len(s%pending)<n.and..not.s%io_failed)
            call more(s)
        end do
        if(len(s%pending)<n) then; out=''; return; end if
        out=s%pending(:n); s%pending=s%pending(n+1:)
    end subroutine

    subroutine more(s)
        type(session_t),intent(inout)::s
        character(kind=c_char)::buf(8192)
        character(len=8192)::chunk
        integer(c_int)::n
        integer::i
        n=fx_test_read(s%handle,buf,8192_c_int,10000_c_int)
        if(n<=0) then
            s%io_failed=.true.
            call check(.false.,'server output arrives before timeout')
            return
        end if
        do i=1,n
            chunk(i:i)=buf(i)
        end do
        if(.not.allocated(s%pending)) s%pending=''
        s%pending=s%pending//chunk(:n)
    end subroutine
end program
