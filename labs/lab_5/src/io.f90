module IO
    use Environment
    implicit none

    type String
        character(kind=CH_) :: ch
        type(String), pointer :: next => Null()
    end type String

    type Tree
        character :: ch
        type(Tree),pointer :: left => Null()
        type(Tree),pointer :: right => Null()
    end type Tree

    type node
        class(node), pointer :: next  => Null()
    end type node

    type, extends(node) :: variable
        character(kind=CH_)  :: char = ""
    end type variable

    type, extends(variable) :: operation
    end type operation

    type tree_node
        class(tree_node), pointer :: left  => Null(), right => Null()
    end type tree_node

    type, extends(tree_node) :: tree_variable
        character(kind=CH_)  :: char = ""
    end type tree_variable

    type, extends(tree_variable) :: tree_operation
    end type tree_operation

    character(*),parameter :: format = '(a1)'
    integer :: size = 1

contains

    function Read_list(Input_File) result(List)
        class(node), pointer        :: List
        character(*), intent(in)   :: Input_File
        integer  In

        ! При чтении только английских букв и цифр лучше открывать как ASCII.
        !open (file=Input_File, encoding=E_, newunit=In)
        open (file=Input_File, newunit=In)
        List => Read_value(In)
        close (In)
    end function Read_list

    ! Чтение следующего значения.
    recursive function Read_value(In) result(Elem)
        class(node), pointer  :: Elem
        integer, intent(in)     :: In
        integer  IO

        character(kind=CH_)  :: char = ""
        
        read (In, '(a1)', iostat=IO, advance='no') char
        if (IO == 0) then
        select case (char)
            case (CH__'a':CH__'z', CH__'A':CH__'Z')
                allocate (Elem, source=variable(char=char))
            case (CH__'+', CH__'-', CH__'*', CH__'/')
                allocate (Elem, source=operation(char=char))
            case default
                print *,char," - непредвиденный символ"
        end select
        Elem%next => Read_value(In)
        else
            nullify (Elem)
        end if
    end function Read_value

    subroutine WriteString(Output_File,String_List)
        character(*),intent(in) :: Output_File
        type(String),intent(in) :: String_List
        
        integer :: Out

        open(file=Output_File,newunit=Out,encoding=E_,status="replace")
            call WriteChar(Out,String_List)
        close(Out)
    end subroutine WriteString

    recursive subroutine WriteChar(Out,Str)
        integer :: Out
        type(String) :: Str

        write (out,format,advance='no') Str%ch
        if (associated(Str%next)) then
            call WriteChar(out,Str%next)
        end if
    end subroutine WriteChar
end module IO