program exercise_4
    implicit none
    real, parameter :: A = 0, B = .5
    real, parameter :: h = 0.0025
    integer, parameter :: steps = (B - A) / h
    real :: func_values(steps) = 0
    integer :: i

    do i = 1,steps
        func_values(i) = A + h * i
    end do

    do i = 1,steps
        func_values(i) = func(func_values(i))
    end do

    print *, (h * sum(func_values))

contains
    real function func(x)
        implicit none
            real :: x,res
            real, parameter :: pi = 3.1415927

            res = exp(x) * sin(pi * x + .5)
    end function func

end program exercise_4