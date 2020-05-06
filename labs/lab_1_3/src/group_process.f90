module Group_Process
    use Environment
    use IO

    implicit none

contains
    function UniqueNames(Group) result(Unique_Names)
        type(student),intent(in) :: Group(STUD_AMOUNT)
        character(NAME_LEN,kind=CH_),allocatable :: Unique_Names(:)

        logical,allocatable :: Unique_Mask(:)
        integer,allocatable :: Unique_Pos(:)
        integer :: i
        integer, parameter :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
        
        Unique_Mask = [(all(Group(:i-1)%Name /= Group(i)%Name), i=1,STUD_AMOUNT)]
        Unique_Pos = pack(INDEXES,Unique_Mask)
        Unique_Names = [(Group(Unique_Pos(i))%Name, i = 1,Count(Unique_Mask))]
    end function UniqueNames

    function CountEntries(Array, Val_to_search) result(quantity)
        character(NAME_LEN,kind=CH_),intent(in) :: Array(:),Val_to_search
        integer :: quantity
    
        quantity = Count(Array == Val_to_search)
    end function CountEntries
end module Group_Process