fn main()
    xs := array(int, 10)
    i := 0
    while(i < 10)
        set xs[i] = i
        set i = i + 1
    end
    set i = 0
    while(i < 10)
        print_int(xs[i])
        set i = i + 1
    end
end
