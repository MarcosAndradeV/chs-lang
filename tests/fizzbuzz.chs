extern fn puts (s: string) -> int

fn main() -> int
    i := 0
    while(i < 10)
        fizz := i%3 == 0
        buzz := i%5 == 0
        if(fizz)
            _ := puts("Fizz")
        else if(buzz)
            _ := puts("Buzz")
        else
            print_int(i)
        end end
        set i = 1 + i
    end
    0
end
