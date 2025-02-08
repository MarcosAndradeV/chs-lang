use "std.chs"

fn main()
    i := 0
    while(i < 10)
        puts("Hello CHS\n")
        set i = i + 1
    end
    set i = 0
    while(i < 10)
        print_int(i)
        set i = i + 1
    end
end
