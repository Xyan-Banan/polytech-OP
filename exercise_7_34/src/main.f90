program exercise_7_14
    use Environment

    implicit none
    integer, parameter :: N_ = 5
    real :: a(N_,N_), buf
    integer :: i,  maxlocs(N_)

    call random_number(a)
    a = (a * 20) - 10
    maxlocs = maxloc(a,dim=2)
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)
    print *,"------------------"
    print '('//N_//'i2)', (maxlocs(i),i=1,N_)

    do concurrent (i = 1:N_)
        if (maxlocs(i) /= i) then
            buf = a(i,i)
            a(i,i) = a(i,maxlocs(i))
            a(i,maxlocs(i)) = buf
        end if

    end do

    print *,"------------------"
    print '('//N_//'f6.1)', (a(i, :), i=1,N_)


end program exercise_7_14