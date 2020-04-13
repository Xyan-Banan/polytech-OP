program exercise_7_14
    use Environment

    implicit none
    integer, parameter :: N_ = 5
    real :: a(N_,N_), sum_col(N_)
    integer :: i

    ! sum = 0
    call random_number(a)
    a = (a * 20) - 10
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)

    do concurrent (i = 2:N_)
        sum_col(i) = Sum(a(1:i-1,i))
    end do

    print "(a,f5.1)","Sum = ",sum(sum_col)


end program exercise_7_14