program lab_4
    use Environment
    use IO
    use Process
    implicit none
    
    type(Polynome),pointer :: poly_list_P => Null()
    type(Polynome),pointer :: poly_list_Q => Null()
    type(Polynome),pointer :: poly_list_R => Null()
    
    character(:), allocatable  :: input_file, output_file

    input_file = "../data/data.txt"
    output_file = "output.txt"

    call ReadPolynome(input_file,poly_list_P, poly_list_Q)
    if (associated(poly_list_P) .or. associated(poly_list_Q)) then
        call PolySum(poly_list_P,poly_list_Q, poly_list_R)
        if (associated(poly_list_R)) then
            call WritePolynome(output_file,poly_list_R)
        end if
    end if
contains
    recursive subroutine mywritelist(Out,poly_list)
        integer :: Out
        type(Polynome) :: poly_list

        write (Out,'(i0,1x,i0)') poly_list%coeff,poly_list%pow

        if (associated(poly_list%next)) then
            call mywritelist(Out,poly_list%next)
        end if
    end subroutine mywritelist
end program lab_4