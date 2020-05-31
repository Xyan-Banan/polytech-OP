module Process
    use Environment
    use IO
    implicit none

contains
    function PolySum(Poly_P,Poly_Q) result(Poly_R)
        type(Polynome), pointer :: Poly_P,Poly_Q,Poly_R

        if(associated(Poly_P) .and. associated(Poly_P)) then
            if (Poly_P%pow == Poly_Q%pow) then

            else if(Poly_P%pow > Poly_Q%pow) then

            else
                
            end if
        end if
    end function






! subroutine ComplementWithFile(Rec_List,File_Path)
    !     type(Record),intent(inout),pointer :: Rec_List
    !     character(*),intent(in) :: File_Path

    !     type(Record),pointer :: Rec_Comp_List

    !     Rec_Comp_List => ReadRecordList(File_Path)

    !     call Complement(Rec_List,Rec_Comp_List)
    ! end subroutine ComplementWithFile

    ! pure recursive subroutine Complement(Rec_List_A, Rec_List_B)
    !     type(Record),intent(inout),pointer :: Rec_List_A
    !     type(Record),intent(in) :: Rec_List_B

    !     if(ValInSet(Rec_List_A,Rec_List_B)) then !есть ли текущий элемент А в множестве знаечений В
    !         !текущий элемент А есть в множестве В, его надо удалить
    !         if (associated(Rec_List_A%next)) then       !есть ли следующий элемент
    !             Rec_List_A = Rec_List_A%next            !сдвигаем следующий элемент на место текущего
    !             call Complement(Rec_List_A,Rec_List_B)  !проверяем новый текущий элемент
    !         else
    !             deallocate(Rec_List_A)                  !т.к. следующего нет, текущий просто удалить
    !             nullify(Rec_List_A)
    !         end if
    !     else
    !         !текущего элемента А нет в множестве В, его пропускаем
    !         if (associated(Rec_List_A%next)) then       !если есть следующий элемент в списке, проверяем его
    !             call Complement(Rec_List_A%next,Rec_List_B)
    !         end if
    !     end if
    ! end subroutine Complement

    ! pure recursive function ValInSet(Rec,Rec_List) result(res)
    !     type(Record),intent(in) :: Rec,Rec_List
    !     logical :: res

    !     if (IsEqual(Rec,Rec_List)) then
    !         res = .true.
    !     else
    !         if (associated(Rec_List%next)) then
    !             res = ValInSet(Rec,Rec_List%next)
    !         else
    !             res = .false.
    !         end if
    !     end if
    ! end function ValInSet

    ! pure logical function IsEqual(Rec1,Rec2)
    !     type(Record),intent(in) :: Rec1,Rec2
        
    !     if (Rec1%Surname == Rec2%Surname .and. Rec1%Name == Rec2%Name) then
    !         IsEqual = .true.
    !     else
    !         IsEqual = .false.
    !     end if
    ! end function IsEqual
end module Process