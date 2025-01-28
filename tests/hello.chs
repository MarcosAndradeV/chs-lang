fn main()
    msg := "Hello from CHS\n"
    _ := syscall(1, 1, msg, len(msg))
end
