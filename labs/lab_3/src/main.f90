program lab_1_5
    use Environment
    use IO
    use Set_Process
    implicit none
    
    type(Record),pointer :: rec_list => Null()
    ! type(Record),pointer :: Rec_List_B => Null()

    character(:), allocatable  :: F1, F2, output_file

    F1 = "../data/F1.txt"
    F2 = "../data/F2.txt"
    output_file = "output.txt"

    rec_list => ReadRecordList(F1)

    if (associated(rec_list)) then
        open(OUTPUT_UNIT,encoding=E_)
        ! call mywritelist(OUTPUT_UNIT,rec_list)
        call ComplementWithFile(rec_list,F2)
        if(associated(rec_list)) then
            call WriteRecordList(output_file,rec_list)
        else
            print *,"В списке не осталось записей"
        end if
    end if
contains
    recursive subroutine mywritelist(Out,Rec_List)
        integer :: Out
        type(Record) :: Rec_List

        write (out,format) Rec_List%Surname, Rec_List%Name

        if (associated(Rec_List%next)) then
            call mywritelist(out,Rec_List%next)
        end if
    end subroutine mywritelist
end program lab_1_5