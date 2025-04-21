extern fn printf(s: string, ...) -> int

fn main() -> int
    acc := 0i32
    a := 999i32
    while(a > 100)
        b := 999i32
        while(b > 100)
            c := a * b
            if(is_pal(c))
                if(acc<c)
                    acc = c
                end
            end
            b = b - 1
        end
        a = a - 1
    end
    printf("Answer: %d\n", acc)
    return 0
end

fn is_pal(i: int) -> bool
    a := i
    b := 0i32
    while(a > 0)
        b = (10 * b)
        b = b + (a % 10)
        a = a/10
    end
    return i == b
end
