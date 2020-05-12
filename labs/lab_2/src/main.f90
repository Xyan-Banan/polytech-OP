program lab_1_5
    use Environment
    use IO
    use String_Process
    implicit none
    
    type(String),pointer :: string_list_A => Null()
    type(String),pointer :: string_list_B => Null()

    character(:), allocatable  :: input_file, output_file

    input_file = "../data/names.txt"
    output_file = "output.txt"

    string_list_A => ReadString(input_file)

    if (associated(string_list_A)) then
        ! open(OUTPUT_UNIT,encoding=E_)
        ! call mywritelist(OUTPUT_UNIT,string_list_A)
        ! print *,""
        string_list_B => SubstringAt(string_list_A,5)
        if (associated(string_list_B)) then
            ! call mywritelist(OUTPUT_UNIT,string_list_B)
            call WriteString(output_file,string_list_B)
        else
            print *,"Неверно задано начало подстроки"
        end if
    end if
contains
    recursive subroutine mywritelist(out,string_list)
        integer :: out
        type(String) :: string_list

        write (out,format,advance='no') string_list%ch

        if (associated(string_list%next)) then
            call mywritelist(out,string_list%next)
        end if
    end subroutine mywritelist
end program lab_1_5