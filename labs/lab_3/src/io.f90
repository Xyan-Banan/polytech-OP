module IO
    use Environment
    implicit none

    integer,parameter :: STR_LEN = 10

    type Record
        character(STR_LEN,kind=CH_) :: Surname = ""
        character(STR_LEN,kind=CH_) :: Name = ""
        type(Record),pointer        :: next => Null()
    end type Record

    character(*),parameter :: format = '(a,1x,a)'

contains
    function ReadRecordList(Input_File) result(Record_List)
        character(*),intent(in)  :: Input_File
        type(Record),pointer    :: Record_List

        integer :: In
        
        open(file=Input_File,encoding=E_,newunit=In)
            Record_List => ReadRecord(In)
        close(In)
    end function ReadRecordList

    recursive function ReadRecord(In) result(rec)
        integer,intent(in)    :: In
        type(Record),pointer :: rec

        integer :: IO
        
        allocate(rec)
        read(In,format,iostat=IO) rec%Surname, rec%Name
        if (IO == 0) then
            rec%next => ReadRecord(In)
        else
            deallocate(rec)
            nullify(rec)
        end if
    end function ReadRecord

    subroutine WriteRecordList(Output_File,Record_List)
        character(*),intent(in) :: Output_File
        type(Record),intent(in) :: Record_List
        
        integer :: Out

        open(file=Output_File,newunit=Out,encoding=E_,status="replace")
            call WriteRecord(Out,Record_List)
        close(Out)
    end subroutine WriteRecordList

    recursive subroutine WriteRecord(Out,Rec)
        integer :: Out
        type(Record) :: Rec

        write (out,format) Rec%Surname,Rec%Name
        if (associated(Rec%next)) then
            call WriteRecord(out,Rec%next)
        end if
    end subroutine WriteRecord
end module IO