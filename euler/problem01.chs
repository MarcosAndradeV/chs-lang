extern fn printf(s: string, ...) -> int

fn main() -> int
    acc := 0i32
    i := 0i32
    while(i < 1000)
        if(i%3 == 0 || i%5 == 0)
            acc = acc + i
        end
        i = i + 1
    end
    printf("The sum is: %d", acc)
    return 0;
end
