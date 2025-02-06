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
    _ := syscall(1, 1, msg, strlen(msg))
end

fn strlen(str: string) -> int
    s: *char = str
    while(*s != '\0') set s = s + 1 end
    cast(int)s - cast(int)str
end
