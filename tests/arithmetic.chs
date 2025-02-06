fn main()
    puts("Addition: ")
    print_int(10 + 20)

    puts("Subtraction: ")
    print_int(30 - 20)

    puts("Multiplication: ")
    print_int(20 * 10)

    puts("Division: ")
    print_int(30 / 10)

    puts("Modulo: ")
    print_int(0) #5 % 10)
end

fn puts(s: string)
    _ := syscall(1, 1, s, strlen(s))
end

fn strlen(str: string) -> int
    s: *char = str
    while(*s != '\0') set s = s + 1 end
    cast(int)s - cast(int)str
end
