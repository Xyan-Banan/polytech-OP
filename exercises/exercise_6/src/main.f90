program exercise_6
    implicit none
    real :: prev_z, next_z, a
    real, parameter :: ABSERR = epsilon(1.)

    print *,"Введите число, корень которого хотите вычислить"
    read *, a

    prev_z = a
    next_z = getNextZ(prev_z, a)

    do while( abs(next_z - prev_z) >= ABSERR)
        prev_z = next_z
        next_z = getNextZ(prev_z, a)
        print *,prev_z
    end do

    print *,"Корень вашего числа: ", next_z

contains
    function getNextZ(prev_z, a) result(res)
        real :: prev_z,a,res
        res = prev_z - NutonFunc(prev_z, a)/NutonFuncDer(prev_z)
    end function getNextZ

    function NutonFunc(x, a) result(res)
        real :: x, a, res
        res = x*x - a
    end function NutonFunc

    function NutonFuncDer(x) result(res)
        real :: x, res
        res = x*2
    end function NutonFuncDer

end program exercise_6