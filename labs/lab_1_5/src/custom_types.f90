module Custom_Types
    use Environment
    implicit none

    integer, parameter  :: SURNAME_LEN = 15,&
                           NAME_LEN = 10,&
                           PATRONYMIC_LEN = 15,&
                           STUD_AMOUNT = 12

    type Student
        character(SURNAME_LEN,kind=CH_)     :: Surname      = ""
        character(NAME_LEN,kind=CH_)        :: Name         = ""
        character(PATRONYMIC_LEN,kind=CH_)  :: Patronymic   = ""
        type(Student),pointer               :: next         => Null()
    end type Student

    type Unique_Mask
        logical :: is_uniq = .false.
        type(Unique_Mask),pointer :: next => Null()
    end type Unique_Mask

    type Entries
        integer :: quant = 0
        type(Entries),pointer :: next => Null()
    end type Entries
end module Custom_Types