module IO
    use Environment
    use Custom_types
    implicit none
    character(*),parameter :: format = '(3(a,1x))'
        
contains
    function ReadGroupList(Input_File) result(Group_List)
        character(*),intent(in)  :: Input_File
        type(Student),pointer    :: Group_List

        integer :: In
        
        open(file=Input_File,encoding=E_,newunit=In)
            Group_List => ReadStudent(In)
        close(In)
    end function ReadGroupList

    recursive function ReadStudent(In) result(Stud)
        integer,intent(in)    :: In
        type(Student),pointer :: Stud

        integer :: IO
        
        allocate(Stud)
        read(In,format,iostat=IO) Stud%Surname, Stud%Name, Stud%Patronymic
        if (IO == 0) then
            Stud%next => ReadStudent(In)
        else
            deallocate(Stud)
            nullify(Stud)
        end if
    end function ReadStudent

    subroutine WriteResultData(Output_File,Unique_Mask_List,Group_List,Entries_List)
        character(*),intent(in) :: Output_File
        type(Unique_Mask),intent(in) :: Unique_Mask_List
        type(Student),intent(in) :: Group_List
        type(Entries),intent(in) :: Entries_List
        
        integer :: Out

        open(file=Output_File,newunit=Out,encoding=E_)
            call WriteRecord(Out,Unique_Mask_List,Group_List,Entries_List)
        close(Out)
    end subroutine WriteResultData

    recursive subroutine WriteRecord(Out,Unique_Mask_List,Group_List, Entries_List)
        integer :: Out
        type(Unique_Mask) :: Unique_Mask_List
        type(Student) :: Group_List
        type(Entries) :: Entries_List

        if (Unique_Mask_List%is_uniq) then
            write (out,"(a,a,i0)") Group_List%Name," - ",Entries_List%quant
        end if
        if (associated(Unique_Mask_List%next)) then
            call WriteRecord(out,Unique_Mask_List%next,Group_List%next,Entries_List%next)
        end if
    end subroutine WriteRecord
end module IO