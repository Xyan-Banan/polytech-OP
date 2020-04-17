program exercise_5
    implicit none
    real :: Y(11),rand(1)
    integer :: i

    do i = 1,11
        call random_number(rand)
        Y(i) = 100 * rand(1)
    end do

    print *, Y, Y(minloc(Y))

end program exercise_5