program exercise_2
    implicit none
    
    integer :: x,y,z,u,v;
    
    print *,"Введите целые числа X, Y и Z:"
    read *,x,y,z

    u = max (x,y,z)
    v = min (x,y,z)

    print *, "U = ", u, "; V = ", v

end program exercise_2