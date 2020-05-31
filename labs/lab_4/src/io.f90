module IO
    use Environment
    implicit none

    integer,parameter :: STR_LEN = 10

    type Polynome
        integer :: coeff
        integer :: pow
        type(Polynome),pointer :: next => Null()
    end type Polynome

    character(*),parameter :: format = '("(",i,1x,i,"), ")'

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
        ! print '(i0,1x,i0,1x,i0)',rec%coeff, rec%pow, IO
        if (IO == 0) then
            rec%next => ReadTerm(In)
        else if (IO /= -2 .and. IO /= -1) then
            deallocate(rec)
            nullify(rec)
        end if
    end function ReadTerm

    ! subroutine WriteRecordList(Output_File,Record_List)
    !     character(*),intent(in) :: Output_File
    !     type(Record),intent(in) :: Record_List
        
    !     integer :: Out

    !     open(file=Output_File,newunit=Out,encoding=E_,status="replace")
    !         call WriteRecord(Out,Record_List)
    !     close(Out)
    ! end subroutine WriteRecordList

    ! recursive subroutine WriteRecord(Out,Rec)
    !     integer :: Out
    !     type(Record) :: Rec

    !     write (out,format) Rec%Surname,Rec%Name
    !     if (associated(Rec%next)) then
    !         call WriteRecord(out,Rec%next)
    !     end if
    ! end subroutine WriteRecord
end module IO