program exercise_7_14
    use Environment

    implicit none
    integer, parameter :: N_ = 5
    real :: a(N_,N_), sum_col(N_-1)
    integer :: i

    ! sum = 0
    call random_number(a)
    a = (a * 20) - 10
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)

    do concurrent (i = 1:N_-1)
        sum_col(i) = Sum(a(i+1:N_,i))
    end do

    print "(/"//N_-1//"f6.1)",(sum_col(i), i = 1,N_-1)
    print "(a,f5.1)","Sum = ",sum(sum_col)


end program exercise_7_14