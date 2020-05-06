program lab_1_5
    use Environment
    use IO
    use Group_Process
    implicit none
    
    type(Student),    pointer :: group_list       => Null()
    type(Unique_Mask),pointer :: unique_mask_list => Null()
    type(Entries),    pointer :: entries_list     => Null()

    character(:), allocatable  :: input_file, output_file

    input_file = "../data/names.txt"
    output_file = "output.txt"

    group_list => ReadGroupList(input_file)

    if (associated(group_list)) then
        allocate(unique_mask_list)
        allocate(entries_list)
        call GetUniqueMask(unique_mask_list,group_list)
        call CountEntries(unique_mask_list,group_list,entries_list)
        call WriteResultData(output_file,unique_mask_list,group_list,entries_list)
        ! open(OUTPUT_UNIT,encoding=E_)
        ! call mywritelist(OUTPUT_UNIT,unique_mask_list,group_list,entries_list)
    end if
contains
    recursive subroutine mywritelist(out,uniq,list, entries_list)
        integer :: out
        type(Unique_Mask) :: uniq
        type(Student) :: list
        type(Entries) :: entries_list

        if (uniq%is_uniq) then
            write (out,*) uniq%is_uniq,list%Name,entries_list%quant
        end if
        if (associated(uniq%next)) then
            call mywritelist(out,uniq%next,list%next,entries_list%next)
        end if
    end subroutine mywritelist
end program lab_1_5