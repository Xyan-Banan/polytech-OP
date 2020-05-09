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
        string_list_B => SubstringAt(string_list_A,5)
        ! call WriteString(output_file,string_list_A)
        open(OUTPUT_UNIT,encoding=E_)
        call mywritelist(OUTPUT_UNIT,string_list_A)
        ! call mywritelist(OUTPUT_UNIT,string_list_B)
    end if
contains
    recursive subroutine mywritelist(out,string_list)
        integer :: out
        type(String) :: string_list

        write (out,format) string_list%ch

        if (associated(string_list%next)) then
            call mywritelist(out,string_list%next)
        end if
    end subroutine mywritelist
end program lab_1_5