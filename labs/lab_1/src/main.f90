program lab_1_1
    use Environment
    implicit none
    integer, parameter :: SURNAME_LEN = 15, NAME_LEN = 10, PATRONYMIC_LEN = 15, STUD_AMOUNT = 12
    integer :: In, Out, IO, i
    integer, parameter :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

    character(SURNAME_LEN, kind=CH_)      :: Surnames(STUD_AMOUNT)
    character(NAME_LEN, kind=CH_)         :: Names(STUD_AMOUNT)
    character(PATRONYMIC_LEN, kind=CH_)   :: Patronymics(STUD_AMOUNT)
    
    logical,allocatable                   :: Uniq(:)
    integer,allocatable                   :: Uniq_Pos(:)
    integer                               :: Uniq_Amount

    character(:), allocatable  :: input_file, output_file, format

    input_file = "../data/names.txt"
    output_file = "output.txt"

    open (file=input_file,encoding=E_,newunit=In)
        format = '(3(a,1x))'
        read(In,format,iostat=IO) (Surnames(i), Names(i), Patronymics(i), i = 1, STUD_AMOUNT)
    close(In)

    ! Out = OUTPUT_UNIT !для вывода на консоль кириллицы
    ! open(Out,encoding=E_)
    ! write (Out, format) (Surnames(i), Names(i), Patronymics(i), i = 1, STUD_AMOUNT) 

    Uniq = [(all(Names(:i-1) /= Names(i)), i=1,STUD_AMOUNT)]
    Uniq_Pos = pack(INDEXES,Uniq)
    Uniq_Amount = Count(Uniq)
    ! write(Out,"(i0,1x,a,L)") (i,Names(i),Uniq(i), i = 1,STUD_AMOUNT)

    ! close(Out)

    open(file=output_file,encoding=E_,newunit=Out)
        format = "(a,a,i0)"    
        write(Out,format) (Names(Uniq_Pos(i))," - ",Count(Names == Names(Uniq_Pos(i))), i = 1,Uniq_Amount)
    close(Out)


end program lab_1_1