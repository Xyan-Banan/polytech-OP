program exercise_7_44
    use Environment

    implicit none
    integer, parameter :: N_ = 5
    real :: a(N_,N_), maxvals(N_)
    integer :: i

    call random_number(a)
    a = (a * 20) - 10
    maxvals = maxval(a,dim=1)
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)
    print *,"------------------"
    print '('//N_//'f6.1)', (maxvals(i),i=1,N_)

end program exercise_7_44