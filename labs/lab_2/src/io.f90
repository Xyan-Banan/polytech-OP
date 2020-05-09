module IO
    use Environment
    implicit none

    type String
        character(kind=CH_) :: ch
        type(String), pointer :: next => Null()
    end type String

    character(*),parameter :: format = '(a)'
        
contains
    function ReadString(Input_File) result(String_List)
        character(*),intent(in) :: Input_File
        type(String),pointer    :: String_List

        integer :: In
        
        open(file=Input_File,encoding=E_,newunit=In)
            String_List => ReadChar(In)
        close(In)
    end function ReadString

    recursive function ReadChar(In) result(Str)
        integer,intent(in)    :: In
        type(String),pointer :: Str

        integer :: IO
        
        allocate(Str)
        read(In,format,iostat=IO) Str%ch
        
        if (IO == 0) then
            open(OUTPUT_UNIT,encoding=E_)
            !     write (OUTPUT_UNIT,*) "ReadChar ",Str%ch
            ! close(OUTPUT_UNIT)
            print *,"read char",Str%ch
            Str%next => ReadChar(In)
        else
            deallocate(Str)
            nullify(Str)
        end if
    end function ReadChar

    subroutine WriteString(Output_File,String_List)
        character(*),intent(in) :: Output_File
        type(String),intent(in) :: String_List
        
        integer :: Out

        open(file=Output_File,newunit=Out,encoding=E_)
            call WriteChar(Out,String_List)
        close(Out)
    end subroutine WriteString

    recursive subroutine WriteChar(Out,Str)
        integer :: Out
        type(String) :: Str

        write (out,format) Str%ch
        if (associated(Str%next)) then
            call WriteChar(out,Str%next)
        end if
    end subroutine WriteChar
end module IO