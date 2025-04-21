extern fn printf(s: string, ...) -> int

fn main() -> int
    i := 0i32;
    while(i<10) i = i + 1;
        if(i % 2 == 0)
            printf("Fizz\n");
        else
            if (i % 3 == 0)
                printf("Buzz\n");
            else
                if (i % 5 == 0)
                    printf("FizzBuzz\n");
                else
                    printf("%d\n", i);
                end
            end
        end
    end
    return 0;
end
