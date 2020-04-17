program exercise_3
    implicit none
    
    integer, parameter :: N_ = 2, M_ = 3
    integer :: arr(N_,M_)
    integer :: X(N_) = 0
    integer :: Y(M_) = 0
    integer :: i,j
    real :: rand(1)

    ! arr = [((nint(100 * rand(1)), j = 1,50), i = 1,20)]

    do i = 1,N_
        do j = 1,M_
            call random_number(rand)

            arr(i,j) = nint(100 * rand(1))

            X(i) = X (i) + arr(i,j)
            Y(j) = Y (j) + arr(i,j)
        end do
        print "(20i4)", (arr(i,j), j = 1,M_)        
        
    end do

    print *, "Суммы по строкам: ", X
    print *, "Суммы по столбцам: ", Y
    ! arr = [(((arr(i,j) * 100.), j = 1,M_), i = 1,N_)]

    ! print "(3f3.2,/,3f3.2)", ((arr(i,j),j = 1,M_),i = 1,N_)

end program exercise_3