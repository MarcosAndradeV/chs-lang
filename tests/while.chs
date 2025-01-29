fn main()
    i := 0
    while(i < 10)
        print("CHS\n")
        set i = i + 1
    end
end

fn print(msg: string)
    _ := syscall(1, 1, msg, len(msg))
end
