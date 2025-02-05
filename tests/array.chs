fn main()
    xs := array(int, 10)
    i := 0
    while(i<10)
        set *(xs + i * 8) = i
        set i = i + 1
    end
    set i = 0
    while(i<10)
        print_int(*(xs + i * 8))
        set i = i + 1
    end
end
