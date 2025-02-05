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
    print_int(5 % 10)
end

fn puts(s: string)
    _ := syscall(1, 1, s, len s)
end
