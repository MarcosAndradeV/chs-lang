extern fn printf(s: string, ...) -> int

fn main() -> int
    a := 1i32
    b := 2i32
    acc := 0i32
    while(a < 4000000)
        if(a%2 == 0) acc = acc + a end
        c := b
        b = a + b
        a = c
    end
    printf("Answer: %d\n", acc)
    return 0
end
