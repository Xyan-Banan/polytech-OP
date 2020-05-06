module Group_Process
    use Environment
    use IO
    implicit none

contains
    pure recursive subroutine GetUniqueMask(Unique_Mask_List, Group_List)
        type(Unique_Mask),intent(inout),pointer :: Unique_Mask_List
        type(Student),    pointer :: Group_List

        if(.not.associated(Group_List%next)) then
            Unique_Mask_List%is_uniq = .true.
        else
            Unique_Mask_List%is_uniq = CheckUniqueValue(Group_List,Group_List%next)
            allocate(Unique_Mask_List%next)
            call GetUniqueMask(Unique_Mask_List%next,Group_List%next)
        end if
    end subroutine GetUniqueMask

    pure recursive function CheckUniqueValue (Stud_to_check, Group_List) result(check)
        type(Student),intent(in) :: Stud_to_check
        type(Student),intent(in) :: Group_List
        logical                          :: check

        if (Stud_to_check%Name == Group_List%Name) then
            check = .false.
        else
            if (associated(Group_List%next)) then
                check = CheckUniqueValue(Stud_to_check,Group_List%next)
            else
                check = .true.
            end if
        end if
    end function CheckUniqueValue

    ! оболочка для подсчета
    pure subroutine CountEntries(Unique_Mask_List,Group_List_Head,Entries_List)
        type(Unique_Mask),intent(in) :: Unique_Mask_List
        type(Student),intent(in) :: Group_List_Head
        type(Entries),intent(inout) :: Entries_List

        call CountCurStudEntries(Unique_Mask_List, Group_List_Head, Group_List_Head, Entries_List)
    end subroutine CountEntries

    ! рекусрсивная функция для подсчета каждого уникального значения
    pure recursive subroutine CountCurStudEntries(Unique_Mask_List, Cur_Stud, Group_List_Head, Entries_List)
        type(Unique_mask),intent(in)    :: Unique_Mask_List
        type(Student),    intent(in)    :: Cur_Stud,Group_List_Head
        type(Entries),    intent(inout) :: Entries_List

        if(Unique_Mask_List%is_uniq) then
            call CountValEntries(Cur_Stud, Group_List_Head,Entries_List%quant)
        end if
        if(associated(Cur_Stud%next)) then
            allocate(Entries_List%next)
            call CountCurStudEntries(Unique_Mask_List%next, Cur_Stud%next, Group_List_Head, Entries_List%next)
        end if
    end subroutine CountCurStudEntries

    ! рекурсивная функция для подщета вхождений в список конкретного значения
    pure recursive subroutine CountValEntries(Val_to_search,List,Counter)
        type(Student),intent(in) :: Val_to_search,List
        integer, intent(inout) :: Counter
    
        if (Val_to_search%Name == List%Name) then
            Counter = Counter + 1
        end if
        if (associated(List%next)) then
            call CountValEntries(Val_to_search,List%next,Counter)
        end if
    end subroutine CountValEntries
end module Group_Process