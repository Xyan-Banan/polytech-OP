program exercise_1
    implicit none
    
    integer, parameter :: STEP = 1
    real :: x = 0., y = 0.
    print *, "Введите X для вычисления тангенса"
    read *,x
    y = tg(x,STEP)
    print *,'tg(x) от вашего X (',x,') равен ',y

contains

    recursive function tg(x,step) result (y)
        implicit none
        integer :: step
        real :: x
        real :: y

        if (step == 7) then
            y = x / step
        else
            y = x / (step - x * tg(x, step + 2))
        end if
    end function tg 

end program 