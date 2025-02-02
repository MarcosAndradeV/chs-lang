fn print(msg: string)
    _ := syscall(1, 1, msg, len(msg))
end

fn mmap_allocate(size: int) -> *void
    cast(*void)syscall(9, 0, size, 3, 34, -1, 0)
end

fn main()
    x := cast(*int)mmap_allocate(8)
    set *x = 10
    print_int(*x)
end
