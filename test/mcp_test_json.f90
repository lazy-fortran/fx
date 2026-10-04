module mcp_test_json
    implicit none
    private
    integer, parameter :: MAX_TOKENS = 6000
    integer, parameter :: K_OBJECT=1, K_ARRAY=2, K_STRING=3, K_ATOM=4
    type :: token_t
        integer :: kind=0, parent=0
        character(len=80) :: key=' '
        character(len=256) :: value=' '
    end type
    type, public :: document_t
        type(token_t) :: token(MAX_TOKENS)
        integer :: n=0
        logical :: valid=.false.
        logical :: failed=.false.
    end type
    public :: parse_json, child, string_is, atom_is, array_size
contains
    subroutine parse_json(text, doc)
        character(len=*), intent(in) :: text
        type(document_t), intent(out) :: doc
        integer :: pos
        doc%n=0; doc%valid=.false.; doc%failed=.false.; pos=1
        call ws(text,pos)
        call value(text,pos,0,' ',doc)
        call ws(text,pos)
        doc%valid = doc%n > 0 .and. pos > len(text) .and. .not.doc%failed
    end subroutine

    recursive subroutine value(text,pos,parent,key,doc)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        integer, intent(in) :: parent
        character(len=*), intent(in) :: key
        type(document_t), intent(inout) :: doc
        integer :: ix
        character(len=256) :: s, k
        call ws(text,pos)
        if (pos>len(text) .or. doc%n>=MAX_TOKENS) then
            doc%failed=.true.; return
        end if
        select case(text(pos:pos))
        case('{')
            ix=add(doc,K_OBJECT,parent,key,''); pos=pos+1; call ws(text,pos)
            if (pos<=len(text)) then
                if (text(pos:pos)=='}') then; pos=pos+1; return; end if
            end if
            do
                call ws(text,pos); call quoted(text,pos,k)
                if (pos<1) then; doc%failed=.true.; return; end if
                call ws(text,pos)
                if (pos>len(text)) then; doc%failed=.true.; return; end if
                if (text(pos:pos)/=':') then; doc%failed=.true.; return; end if
                pos=pos+1; call value(text,pos,ix,trim(k),doc)
                if(doc%failed.or.pos<1) return
                call ws(text,pos)
                if (pos>len(text)) then; doc%failed=.true.; return; end if
                if (text(pos:pos)=='}') then; pos=pos+1; exit; end if
                if (text(pos:pos)/=',') then; doc%failed=.true.; return; end if
                pos=pos+1
            end do
        case('[')
            ix=add(doc,K_ARRAY,parent,key,''); pos=pos+1; call ws(text,pos)
            if (pos<=len(text)) then
                if (text(pos:pos)==']') then; pos=pos+1; return; end if
            end if
            do
                call value(text,pos,ix,' ',doc)
                if(doc%failed.or.pos<1) return
                call ws(text,pos)
                if (pos>len(text)) then; doc%failed=.true.; return; end if
                if (text(pos:pos)==']') then; pos=pos+1; exit; end if
                if (text(pos:pos)/=',') then; doc%failed=.true.; return; end if
                pos=pos+1
            end do
        case('"')
            call quoted(text,pos,s)
            if(pos<1) then; doc%failed=.true.; return; end if
            ix=add(doc,K_STRING,parent,key,trim(s))
        case default
            ix=pos
            do while(pos<=len(text))
                if(index(' ,:'//achar(9)//achar(10)//achar(13)//'}]',text(pos:pos))>0) exit
                pos=pos+1
            end do
            if(pos==ix) return
            s=text(ix:min(pos-1,ix+255)); ix=add(doc,K_ATOM,parent,key,trim(s))
        end select
    end subroutine

    integer function add(doc,kind,parent,key,val) result(ix)
        type(document_t), intent(inout) :: doc
        integer, intent(in) :: kind,parent
        character(len=*), intent(in) :: key,val
        if(doc%n>=MAX_TOKENS) then; ix=0; return; end if
        doc%n=doc%n+1; ix=doc%n
        doc%token(ix)%kind=kind; doc%token(ix)%parent=parent
        doc%token(ix)%key=key; doc%token(ix)%value=val
    end function

    subroutine quoted(text,pos,out)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        character(len=*), intent(out) :: out
        integer :: n
        out=' '; n=0
        if(pos>len(text)) return
        if(text(pos:pos)/='"') then; pos=-1; return; end if
        pos=pos+1
        do while(pos<=len(text))
            if(text(pos:pos)=='"') then; pos=pos+1; return; end if
            if(text(pos:pos)==achar(92)) then
                pos=pos+1; if(pos>len(text)) exit
                select case(text(pos:pos))
                case('"','\','/'); n=n+1; if(n<=len(out)) out(n:n)=text(pos:pos)
                case('n'); n=n+1; if(n<=len(out)) out(n:n)=achar(10)
                case('r'); n=n+1; if(n<=len(out)) out(n:n)=achar(13)
                case('t'); n=n+1; if(n<=len(out)) out(n:n)=achar(9)
                case default; pos=-1; return
                end select
            else
                n=n+1; if(n<=len(out)) out(n:n)=text(pos:pos)
            end if
            pos=pos+1
        end do
        pos=-1
    end subroutine

    subroutine ws(text,pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        do while(pos<=len(text))
            if(index(' '//achar(9)//achar(10)//achar(13),text(pos:pos))==0) exit
            pos=pos+1
        end do
    end subroutine

    integer function child(doc,parent,key) result(ix)
        type(document_t), intent(in) :: doc
        integer, intent(in) :: parent
        character(len=*), intent(in) :: key
        integer :: i
        ix=0
        do i=1,doc%n
            if(doc%token(i)%parent==parent .and. trim(doc%token(i)%key)==key) then
                ix=i; return
            end if
        end do
    end function

    logical function string_is(doc,ix,expected)
        type(document_t), intent(in) :: doc
        integer, intent(in) :: ix
        character(len=*), intent(in) :: expected
        string_is=ix>0
        if(ix>0) string_is=doc%token(ix)%kind==K_STRING .and. &
            trim(doc%token(ix)%value)==expected
    end function
    logical function atom_is(doc,ix,expected)
        type(document_t), intent(in) :: doc
        integer, intent(in) :: ix
        character(len=*), intent(in) :: expected
        atom_is=ix>0
        if(ix>0) atom_is=doc%token(ix)%kind==K_ATOM .and. &
            trim(doc%token(ix)%value)==expected
    end function
    integer function array_size(doc,ix)
        type(document_t), intent(in) :: doc
        integer, intent(in) :: ix
        integer :: i
        array_size=0
        if(ix<=0) return
        if(doc%token(ix)%kind/=K_ARRAY) return
        do i=1,doc%n
            if(doc%token(i)%parent==ix) array_size=array_size+1
        end do
    end function
end module
