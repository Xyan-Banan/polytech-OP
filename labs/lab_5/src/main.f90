program lab_1_5
    use Environment
    use IO
    use Process
    implicit none
    
    class(node),pointer :: expression_inf
    character(:), allocatable  :: input_file, output_file, res
    logical :: check

    input_file = "../data/data.txt"
    output_file = "output.txt"

    expression_inf => Read_list(input_file)

    if (associated(expression_inf)) then
        call CheckInfForm(expression_inf, check)
        if(check) then
            call GetPrefFromInf(expression_inf,res)
            call writeresult(output_file,res)
        else
            print *,"Выражение в инфиксной форме записано с ошибкой"
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