program lab_1_1
    use Environment
    use IO
    use Group_Process
    implicit none
    integer :: i

    type(student) :: Group(STUD_AMOUNT)
    character(NAME_LEN,kind=CH_),allocatable :: Unique_Names(:)
    integer, allocatable :: Entries(:)

    character(:), allocatable  :: input_file, output_file, data_file

    input_file = "../data/names.txt"
    data_file = "class.dat"
    output_file = "output.txt"

    call CreateDataFile(input_file,data_file)
    Group = ReadFromDataFile(data_file)
    Unique_Names = UniqueNames(Group)
    Entries = [(CountEntries(Group%Name,Unique_Names(i)), i = 1,Size(Unique_Names))]
    call WriteResultData(output_file,Unique_Names,Entries)
end program lab_1_1