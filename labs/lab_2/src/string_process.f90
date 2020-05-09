module String_Process
    use Environment
    use IO
    implicit none

contains
    function SubstringAt(String_List,N) result(Str)
        type(String) :: String_List
        integer      :: N
        type(String),pointer :: Str

        integer :: Counter = 1

        Str => GetCharAt(String_List,N,Counter)
    end function SubstringAt

    recursive function GetCharAt(String_List,N,Counter) result(Str)
        type(String) :: String_List
        integer      :: N
        integer :: Counter

        type(String),pointer :: Str

        open(OUTPUT_UNIT,encoding=E_)
            write (OUTPUT_UNIT,*) "GetCharAt",Counter,N,String_List%ch
        close(OUTPUT_UNIT)

        if (Counter > N) then
            Str => Null()
        else
            if (N == Counter) then
                Str = String_List
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