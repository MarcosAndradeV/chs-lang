fn main()
    msg := "Hello CHS\n"
    i := 0
    while(i < 10)
        print(msg)
        set i = i + 1
    end
    set i = 0
    while(i < 10)
        print_int(i)
        set i = i + 1
    end
end

fn print(msg: string)
    _ := syscall(1, 1, msg, len msg)
end
