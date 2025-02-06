fn main()
    msg := "Hello from CHS\n"
    _ := syscall(1, 1, msg, strlen(msg))
end

fn strlen(str: string) -> int
    s: *char = str
    while(*s != '\0') set s = s + 1 end
    cast(int)s - cast(int)str
end
