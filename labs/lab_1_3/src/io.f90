module IO
    use Environment
    implicit none
    integer, parameter  :: SURNAME_LEN = 15,&
                           NAME_LEN = 10,&
                           PATRONYMIC_LEN = 15,&
                           STUD_AMOUNT = 12
    character(*),parameter :: format = '(3(a,1x))'
    integer, parameter     :: recl = (SURNAME_LEN + NAME_LEN + PATRONYMIC_LEN) * CH_
    
    type Student
        character(SURNAME_LEN,kind=CH_)     :: Surname      = ""
        character(NAME_LEN,kind=CH_)        :: Name         = ""
        character(PATRONYMIC_LEN,kind=CH_)  :: Patronymic   = ""
    end type Student

contains
    subroutine CreateDataFile(Input_File, Data_File)
        character(*),intent(in)  :: Input_File,Data_File
        
        type(Student) :: stud
        integer :: In,Out,i
        
        open(file=Input_File,encoding=E_,newunit=In)
        open(file=Data_File,&
             form="unformatted",&
             newunit=Out,&
             access="direct",&
             recl=recl)
            do i = 1,STUD_AMOUNT
                read(In,format) stud
                write(Out,rec=i) stud
            end do
        close(In)
        close(Out)
    end subroutine CreateDataFile

    function ReadFromDataFile(Data_File) result (Group)
        character(*),intent(in) :: Data_File
        type(Student) :: Group(STUD_AMOUNT)

        integer :: In
        open(file=Data_File,&
             form="unformatted",&
             newunit=In,&
             access="direct",&
             recl=recl*STUD_AMOUNT)
            read (In, rec=1) Group
        close(In)
    end function ReadFromDataFile

    subroutine WriteResultData(Output_File,Unique_Names, Entries)
        character(*),intent(in) :: Output_File
        character(NAME_LEN,kind=CH_),intent(in) :: Unique_Names(:)
        integer,intent(in) :: Entries(:)
        
        integer :: Out,i

        open(file=Output_File,newunit=Out,encoding=E_)
            write (Out, "(a,a,i0)") (Unique_Names(i)," - ",Entries(i), i = 1,Size(Unique_Names))
        close(Out)
    end subroutine WriteResultData
end module IO