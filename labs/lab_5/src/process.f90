module Process
    use Environment
    use IO
    implicit none

contains
    pure subroutine CheckInfForm(Expression_Inf, Check)
        class(node),intent(inout),pointer :: Expression_Inf
        logical,intent(inout) :: Check

        call CheckInfFormInner(Expression_Inf, null(), Check)
    end subroutine CheckInfForm

    pure recursive subroutine CheckInfFormInner(Expression_Inf, Previous_Node, Res)
        class(node),intent(in),pointer :: Expression_Inf
        class(node),intent(in),pointer :: Previous_Node

        logical,intent(inout) :: Res

        if(associated(Expression_Inf)) then
            select type (Expression_Inf)
                type is (variable)
                    if (associated(Previous_Node)) then !если текущий - переменная и есть предыдущий-операция -- проверяем дальше
                        select type (Previous_Node)
                            type is (operation)
                                call CheckInfFormInner(Expression_Inf%next,Expression_Inf, Res)
                            class default
                                Res = .false.
                        end select
                    else
                        call CheckInfFormInner(Expression_Inf%next,Expression_Inf, Res) !если текущий - переменная, а предыдущего нет -- проверяем дальше
                    end if
                type is (operation)
                    if (associated(Previous_Node)) then !если текущий - операция и есть предыдущий-переменная -- проверяем дальше
                        select type (Previous_Node)
                            type is (variable)
                                call CheckInfFormInner(Expression_Inf%next,Expression_Inf, Res)
                            class default
                                Res = .false.
                        end select
                    else
                        Res = .false.
                    end if
            end select
        else
            if (associated(Previous_Node)) then !если текущий - хвост и есть предыдущий-операция -- возвращаем ложь
                select type (Previous_Node)
                    type is (operation)
                        Res = .false.
                    class default
                        Res = .true.
                end select
            else
                Res = .true.
            end if
        end if
    end subroutine CheckInfFormInner

    pure subroutine GetPostFromInf(Expression_Inf, Expression_Post) 
        class(node),intent(inout),pointer :: Expression_Inf
        character(:),allocatable,intent(out) :: Expression_Post

        class(node),pointer :: Expression_Pref
        class(tree_node),pointer :: Expression_Tree

        call GetPrefFromInf(Expression_Inf,Expression_Pref)
        call GetTreeFromPref(Expression_Pref,Expression_Tree)
        Expression_Post = GetPostFromTree(Expression_Tree)
    end subroutine

    pure recursive subroutine GetTreeFromPref(Expression_Pref,Expression_Tree)
        class(node),intent(in),pointer :: Expression_Pref
        class(tree_node),intent(inout),pointer :: Expression_Tree

        select type(Expression_Pref)
            type is (operation)
                allocate(Expression_Tree,source=tree_variable(char=Expression_Pref%char))
                call GetTreeFromPref(Expression_Pref%next,Expression_Tree%right)
                call GetTreeFromPref(Expression_Pref%next%next,Expression_Tree%left)
            type is (variable)
                allocate(Expression_Tree,source=tree_variable(char=Expression_Pref%char))
        end select
    end subroutine

    pure subroutine GetPrefFromInf(Expression_Inf,Expression_Pref)
        class(node),intent(in),pointer :: Expression_Inf
        class(node),intent(inout),pointer :: Expression_Pref !also known as output queue :^)

        class(node),pointer :: Expr_Pointer
        type(String),pointer :: Stack
        character(kind=CH_) :: val

        select type (Expression_Inf)
            type is (variable)
                allocate(Expr_Pointer,source=variable(char=Expression_Inf%char))
            type is (operation)
                allocate(Expr_Pointer,source=operation(char=Expression_Inf%char))
        end select

        do while (associated(Expr_Pointer))
            select type (Expr_Pointer)
                type is (variable)
                    call PushQ(Expression_Pref,Expr_Pointer%char)
                type is (operation)
                    if (associated(Stack)) then
                        do while(associated(Stack) .and. IsOpGrE(Stack%ch,Expr_Pointer%char))
                            call PopS(Stack,val)
                            call PushQ(Expression_Pref,val)
                        end do
                        call PushS(Stack,Expr_Pointer%char)
                    end if
            end select
        end do
        do while(associated(Stack))
            call PopS(Stack,val)
            call PushQ(Expression_Pref,val)
        end do
        
    end subroutine GetPrefFromInf

    pure recursive function GetPostFromTree(Expression) result(res_expr)
        class(tree_node),intent(in) :: Expression
        
        character(:,kind=CH_),allocatable :: res_expr
        character(kind=CH_) :: val

        if(associated(Expression%left) .and. associated(Expression%right)) then
            select type(Expression)
                type is(tree_variable)
                    val = Expression%char
                type is(tree_operation)
                    val = Expression%char
            end select
            res_expr = GetPostFromTree(Expression%left)  // CH__" " // GetPostFromTree(Expression%right) // CH__" " // val
        else 
            select type(Expression)
                type is(tree_variable)
                    res_expr = Expression%char
                type is(tree_operation)
                    res_expr = Expression%char
            end select
        end if
    end function GetPostFromTree

    pure logical function IsOpGrE(Op_A, Op_B)
        character(kind=CH_),intent(in) :: Op_A, Op_B

        select case(Op_A)
            case(CH__'+',CH__'-')
                if(Op_B == CH__'+' .or. Op_B == CH__'-') then
                    IsOpGrE = .true.
                else
                    IsOpGrE = .false.
                end if
            case(CH__'*',CH__'/')
                IsOpGrE = .true.
        end select
    end function IsOpGrE

    pure subroutine PushQ(list,char)
        class(node),intent(inout),pointer :: list
        character(kind=CH_),intent(in) :: char

        class(node),pointer :: Elem

        select case (char)
        case (CH__'a':CH__'z', CH__'A':CH__'Z')
            allocate (Elem, source=variable(char=char))
        case (CH__'+', CH__'-', CH__'*', CH__'/')
            allocate (Elem, source=operation(char=char))
        end select

        Elem%next => list
        list => Elem
    end subroutine PushQ

    pure subroutine PushS(list,char)
        type(String),intent(inout),pointer :: list
        character(kind=CH_),intent(in) :: char

        type(String),pointer :: Elem

        allocate (Elem)
        Elem%ch=char
        Elem%next => list
        list => Elem
    end subroutine PushS

    pure subroutine PopS(list,val)
        type(String),intent(inout),pointer :: list
        character(kind=CH_),intent(out) :: val

        type(string),pointer :: tmp
        if (associated(list)) then
            val = list%ch
            tmp => list
            list => list%next
            deallocate(tmp)
        end if
    end subroutine PopS
    
    recursive pure subroutine PopQ(list,val)
        type(String),pointer :: list
        character,intent(out) :: val

        if(associated(list%next)) then
            call PopQ(list,val)
        else
            val = list%ch
            deallocate(list)
            nullify(list)
        end if
    end subroutine PopQ

    

! (2-3*4)-5

! если (
!     создать новую вершину
!     вызвать рекурсивно для левой ветви
!     прочитать символ
!     присвоить его текущей вершины
!     вызвать для правой ветви
!     прочитать скобку
!     если не )
!         создать новый узел
!         занести прочитанный символвы
end module Process