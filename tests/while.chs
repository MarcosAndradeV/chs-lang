extern fn printf(s: string, ...) -> int

fn main() -> int
    i := 0i32;
    while(i<10)
        printf("i = %d\n", i);
        i = i + 1;
    end
    return 0;
end
