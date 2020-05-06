program lab_1_2
    use Environment
    implicit none
    integer, parameter :: SURNAME_LEN = 15, NAME_LEN = 10, PATRONYMIC_LEN = 15, STUD_AMOUNT = 12
    character(kind=CH_) :: Surnames(STUD_AMOUNT,SURNAME_LEN), Names(STUD_AMOUNT,NAME_LEN), Patronymics(STUD_AMOUNT,PATRONYMIC_LEN)                                
    character(kind=CH_),allocatable      :: Unique_Names(:,:)
    integer,allocatable                  :: Entries_Amount(:)
    integer                              :: Unique_Amount

    character(:), allocatable  :: input_file, output_file

    input_file = "../data/names.txt"
    output_file = "output.txt"

    call GetClassData(input_file,Surnames,Names,Patronymics)
    call UniqueNames(Names,Unique_Names,Unique_Amount)
    call CountEntries(Names,Unique_Names,Unique_Amount,Entries_Amount)
    call WriteResults(output_file,Unique_Names,Unique_Amount,Entries_Amount)

contains
    subroutine GetClassData(Input_File, Surnames,Names,Patronymics)
        character(*),        intent(in)  :: Input_File
        character(kind=CH_), intent(out) :: Surnames(:,:),&
                                            Names(:,:),&
                                            Patronymics(:,:)
        integer :: In,i
        character(:),allocatable :: format
        
        open (file=input_file,encoding=E_,newunit=In)
            format = '('//SURNAME_LEN//'a1,1x,'//NAME_LEN//'a1,1x,'//PATRONYMIC_LEN//'a1)'
            read(In,format) (Surnames(i,:), Names(i,:), Patronymics(i,:), i = 1, STUD_AMOUNT)
        close(In)
    end subroutine GetClassData

    subroutine UniqueNames(Names,Unique_Names,Unique_Amount)
        character(kind=CH_),intent(in) :: Names(:,:)
        character(kind=CH_),allocatable, intent(out) :: Unique_Names(:,:)
        integer,intent(out) :: Unique_Amount

        logical,allocatable :: Unique_Mask(:)
        integer,allocatable :: Unique_Pos(:)
        integer :: i,j
        integer, parameter :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
        
        ! Unique_Mask = [(all([(Names(j,:), j = 1,i-1)] /= Names(i,:)), i=1,STUD_AMOUNT)]
        Unique_Mask = [(.not.any([(all(Names(i,:) == Names(j,:)), i = 1, j - 1)]), j = 1,STUD_AMOUNT)]
        Unique_Amount = Count(Unique_Mask)
        Unique_Pos = pack(INDEXES,Unique_Mask)

        ! Unique_Names = [([Names(Unique_Pos(i),:)], i = 1,Unique_Amount)]

        allocate(Unique_Names(Unique_Amount,NAME_LEN))

        do concurrent (i = 1:Unique_Amount)
            Unique_Names(i,:) = Names(Unique_Pos(i),:)
        end do
    end subroutine UniqueNames
    
    subroutine CountEntries(Names,Unique_Names,Unique_Amount,Entries_Amount)
        character(kind=CH_),intent(in) :: Names(:,:),Unique_Names(:,:)
        integer,intent(in) :: Unique_Amount

        integer,intent(out), allocatable :: Entries_Amount(:)
        integer :: i,j

        Entries_Amount = [(Count([(all(Names(j,:) == Unique_Names(i,:)), j = 1,STUD_AMOUNT)]), i = 1,Unique_Amount)]

    end subroutine CountEntries

    subroutine WriteResults(Output_File,Unique_Names,Unique_Amount, Entries_Amount)
        character(*),intent(in) :: Output_File
        character(kind=CH_),intent(in) :: Unique_Names(:,:)
        integer,intent(in) :: Entries_Amount(:),Unique_Amount

        integer :: Out,i
        character(:),allocatable :: format

        open(file=Output_File,encoding=E_,newunit=Out)
            format = "("//NAME_LEN//"a1,a,i0)"    
            write(Out,format) (Unique_Names(i,:)," - ",Entries_Amount(i), i = 1,Unique_Amount)
        close(Out)
    end subroutine WriteResults
end program lab_1_2