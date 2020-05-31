module Process
    use Environment
    use IO
    implicit none

contains
    pure recursive subroutine PolySum(Poly_P,Poly_Q, Poly_R)
        type(Polynome), pointer :: Poly_P,Poly_Q,Poly_R

        allocate(Poly_R)
        if ((associated(Poly_P) .and. associated(Poly_Q)) .and. (Poly_P%pow == Poly_Q%pow)) then !если равны - то сложение
            Poly_R%pow = Poly_P%pow
            Poly_R%coeff = Poly_P%coeff + Poly_Q%coeff
            call PolySum(Poly_P%next, Poly_Q%next, Poly_R%next)
        else if((associated(Poly_P) .and. associated(Poly_Q)) .and. Poly_P%pow > Poly_Q%pow&
                .or.&
                (associated(Poly_P) .and. (.not.associated(Poly_Q)))) then !если степень первого больше - то его коэффициент
            Poly_R%pow = Poly_P%pow
            Poly_R%coeff = Poly_P%coeff
            call PolySum(Poly_P%next, Poly_Q, Poly_R%next) !вызываем рекурсивно для следующего первого и того же второго
        else if((associated(Poly_P) .and. associated(Poly_Q)) .and. Poly_P%pow < Poly_Q%pow&
                .or.&
                ((.not.associated(Poly_P)) .and. associated(Poly_Q))) then    !если степень второго больше - то его коэффициент
            Poly_R%pow = Poly_Q%pow
            Poly_R%coeff = Poly_Q%coeff
            call PolySum(Poly_P, Poly_Q%next, Poly_R%next) !вызываем рекурсивно для того же первого и следующего второго
        else
            deallocate(Poly_R)
            nullify(Poly_R)
        end if
    end subroutine
end module Process