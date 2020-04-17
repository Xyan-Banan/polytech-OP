program exercise_7_22
    use Environment

    implicit none
    integer, parameter :: N_ = 5
    real :: a(N_,N_)
    integer :: i

    call random_number(a)
    a = (a * 20) - 10
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)

    print "(a,2i2)", "maxloc: ", maxloc(a)
    print "(a,2i2)", "minloc: ", minloc(a)
    print "(a,f5.1)","Max - Min = ",maxval(a) - minval(a)


end program exercise_7_22