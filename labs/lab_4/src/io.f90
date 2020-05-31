module IO
    use Environment
    implicit none

    integer,parameter :: STR_LEN = 10

    type Polynome
        integer :: coeff
        integer :: pow
        type(Polynome),pointer :: next => Null()
    end type Polynome

contains
    subroutine ReadPolynome(Input_File, Poly_P, Poly_Q)
        character(*),intent(in)  :: Input_File
        type(Polynome),pointer    :: Poly_P,Poly_Q

        integer :: In
        
        open(file=Input_File,form='FORMATTED',encoding=E_,newunit=In)
            Poly_P => ReadTerm(In)
            Poly_Q => ReadTerm(In)
        close(In)
    end subroutine ReadPolynome

    recursive function ReadTerm(In) result(rec)
        integer,intent(in)    :: In
        type(Polynome),pointer :: rec

        integer :: IO
        character(:),allocatable :: mystr,mystr2
        
        allocate(rec)
        read(In,'(a1,i1,1x,i1,a2,1x)',iostat=IO,advance="no") mystr,rec%coeff, rec%pow,mystr2
        if (IO == 0) then
            rec%next => ReadTerm(In)
        else if (IO /= -2 .and. IO /= -1) then
            deallocate(rec)
            nullify(rec)
        end if
    end function ReadTerm

    subroutine WritePolynome (Output_File,Poly_List)
        character(*),intent(in) :: Output_File
        type(Polynome),intent(in),pointer :: Poly_List
        
        integer :: Out

        open(file=Output_File,newunit=Out,encoding=E_,status="replace")
            call WriteTerm(Out,Poly_List)
        close(Out)
    end subroutine WritePolynome

    recursive subroutine WriteTerm(Out,Term)
        integer :: Out
        type(Polynome),pointer :: Term

        write (out,'("(",i0,1x,i0,")")',advance="no") Term%coeff,Term%pow
        if (associated(Term%next)) then
            write(out,'(a)',advance="no") ", "
            call WriteTerm(out,Term%next)
        end if
    end subroutine WriteTerm
end module IO