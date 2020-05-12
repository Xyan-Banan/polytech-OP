module String_Process
    use Environment
    use IO
    implicit none

contains
    pure function SubstringAt(String_List,N) result(Str)
        type(String),intent(in) :: String_List
        integer,intent(in)      :: N
        type(String),pointer :: Str

        Str => GetCharAt(String_List,N,1)
    end function SubstringAt

    pure recursive function GetCharAt(String_List,N,Counter) result(Str)
        type(String),intent(in) :: String_List
        integer,     intent(in)        :: N
        integer,     intent(in)        :: Counter
        
        type(String),pointer :: Str

        if (Counter > N) then
            Str => Null()
        else
            if (N == Counter) then
                allocate(Str)
                Str = transfer(String_List,Str)
            else
                if (associated(String_List%next)) then
                    Str => GetCharAt(String_List%next,N,Counter + 1)
                else
                    str => Null()
                end if
            end if
        end if
    end function
end module String_Process