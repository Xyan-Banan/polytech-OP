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

    pure subroutine GetPrefFromInf(Expression_Inf, Expression_Pref) 
        class(node),intent(inout),pointer :: Expression_Inf
        character(:),allocatable,intent(out) :: Expression_Pref

        class(node),pointer :: Expression_Post
        class(Stack_Tree),pointer :: Expression_Tree

        call GetPostFromInf(Expression_Inf,Expression_Post)
        call GetTreeFromPost(Expression_Post,Expression_Tree)
        ! print *,"here all ok"
        Expression_Pref = GetPrefFromTree(Expression_Tree%elem)
    end subroutine

    pure subroutine GetPostFromInf(Expression_Inf,Expression_Post)
        class(node),intent(in),pointer :: Expression_Inf
        class(node),intent(inout),pointer :: Expression_Post !also known as output queue :^)

        class(node),pointer :: Expr_Pointer
        type(String),pointer :: Stack
        character(kind=CH_) :: val

        Stack => Null()
        select type (Expression_Inf)
            type is (variable)
                allocate(Expr_Pointer,source=variable(char=Expression_Inf%char,next=Expression_Inf%next))
            type is (operation)
                allocate(Expr_Pointer,source=operation(char=Expression_Inf%char,next=Expression_Inf%next))
        end select

        do while (associated(Expr_Pointer))
            select type (Expr_Pointer)
                type is (variable)
                    ! print *,Expr_Pointer%char
                    call PushQ(Expression_Post,Expr_Pointer%char)
                    ! print *,Expr_Pointer%char," pushed to queue"
                type is (operation)
                    ! print *,Expr_Pointer%char    
                    ! print *,"going to check stack"
                    if (associated(Stack)) then
                        ! print *,"stack is NOT empty"
                        do while(associated(Stack) .and. IsOpGrE(Stack%ch,Expr_Pointer%char))
                            call PopS(Stack,val)
                            ! print *,val," popped from stack"
                            call PushQ(Expression_Post,val)
                            ! print *,val," pushed to queue"
                        end do
                    else
                        ! print *,"stack is empty"
                    end if
                    call PushS(Stack,Expr_Pointer%char)
                    ! print *, Expr_Pointer%char," pushed to stack"
            end select
            Expr_Pointer => Expr_Pointer%next
        end do
        do while(associated(Stack))
            call PopS(Stack,val)
            call PushQ(Expression_Post,val)
            ! print *,val," pushed to queue"
        end do
        
    end subroutine GetPostFromInf

    pure recursive subroutine GetTreeFromPost(Expression_Post, Stack)
        class(node),intent(inout),pointer :: Expression_Post
        type(Stack_Tree),intent(inout),pointer :: Stack

        class(tree_node),pointer :: Tree_Elem
        type(Stack_Tree),pointer :: New_Stack_Elem

        if(associated(Expression_Post%next)) then
            call GetTreeFromPost(Expression_Post%next,Stack)
        end if
        
        select type(Expression_Post)
            type is (variable)
                ! print *, Expression_Post%char
                allocate(Tree_Elem,source=tree_variable(char=Expression_Post%char))
                allocate(New_Stack_Elem,source=Stack_Tree(elem=Tree_Elem,next=Stack)) !push var to stack
                Stack => New_Stack_Elem
            type is (operation)
                ! print *, Expression_Post%char
                allocate(Tree_Elem,source=tree_variable(char=Expression_Post%char))
                
                Tree_Elem%right => Stack%elem       !put top stack elem to right branch
                New_Stack_Elem => Stack             !pop stack
                Stack => Stack%next
                deallocate(New_Stack_Elem)

                Tree_Elem%left => Stack%elem        !put top stack elem to left branch
                New_Stack_Elem => Stack             !pop stack
                Stack => Stack%next
                deallocate(New_Stack_Elem)

                allocate(New_Stack_Elem,source=Stack_Tree(elem=Tree_Elem,next=Stack)) !push new tree elem to stack
                Stack => New_Stack_Elem
        end select
    end subroutine


    pure recursive function GetPrefFromTree(Expression) result(res_expr)
        class(tree_node),intent(in) :: Expression
        
        character(:,kind=CH_),allocatable :: res_expr
        character(:,kind=CH_),allocatable :: left_part
        character(:,kind=CH_),allocatable :: right_part
        character(kind=CH_) :: val

        if(associated(Expression%left) .and. associated(Expression%right)) then
            select type(Expression)
                type is(tree_variable)
                    val = Expression%char
                type is(tree_operation)
                    val = Expression%char
            end select
            
            left_part = GetPrefFromTree(Expression%left) 
            ! print *,"for node ",val," left part is ",left_part 
            right_part = GetPrefFromTree(Expression%right)
            ! print *,"for node ",val," right part is ",right_part
            res_expr = left_part // right_part // val
            ! print *,"total expression is ",res_expr

            ! res_expr = GetPrefFromTree(Expression%left) // GetPrefFromTree(Expression%right) // val !не работает (((
            ! print *,"total expression is ",res_expr
        else 
            select type(Expression)
                type is(tree_variable)
                    res_expr = Expression%char
                type is(tree_operation)
                    res_expr = Expression%char
            end select
        end if
    end function GetPrefFromTree

    pure logical function IsOpGrE(Op_A, Op_B)
        character(kind=CH_),intent(in) :: Op_A, Op_B

        select case(Op_A)
            case(CH__'+',CH__'-')
                if(Op_B == CH__'+' .or. Op_B == CH__'-') then
                    ! print *,Op_A," is greater or equal ",Op_B
                    IsOpGrE = .true.
                else
                    IsOpGrE = .false.
                end if
            case(CH__'*',CH__'/')
                ! print *,Op_A," is greater or equal ",Op_B
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
    
    pure recursive subroutine PopQ(list,val)
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