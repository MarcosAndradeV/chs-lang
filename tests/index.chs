fn main()
    xs: *int = mmap_allocate(80)
    one := 1
    two := 2
    set xs[1] = 10
    set xs[two] = 20
    set xs[3] = 30
    print_int(xs[one])
    print_int(xs[one + one])
    print_int(xs[one + 2])
end


fn mmap_allocate(size: int) -> *void
    cast(*void)syscall(9, 0, size, 3, 34, -1, 0)
end
