program exercise_7_1
    implicit none
    real :: a(10), abs_a(10), buf
    integer :: i,j

    call random_number(a)
    a = (a * 200) - 100
    print "(/10f6.2)", (a(i), i =1,10)
    abs_a = abs(a)
    print "(/10f6.2)", (abs_a(i), i =1,10)

    do j = 1,9
        do i = 1,9-j
            if (abs_a(i) > abs_a(i+1)) then
                buf = a(i)
                a(i) = a(i+1)
                a(i+1) = buf
                buf = abs_a(i)
                abs_a(i) = abs_a(i+1)
                abs_a(i+1) = buf
            end if
        end do
    end do

    print "(/10f6.2)",(a(i), i =1,10)




end program exercise_7_1