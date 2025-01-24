type print_int fn(int)

fn main(argc: uint, argv: **char)
    x: int = 10
    y: int = 20
    z := x + y
    print_int(z)
end
