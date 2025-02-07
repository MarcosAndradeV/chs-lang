fn main()
    acc := 0
    a := 999
    while(a > 100)
        b := 999
        while(b > 100)
            c := a * b
            if(is_pal(c))
                if(acc<c)
                    set acc = c
                end
            end
            set b = b - 1
        end
        set a = a - 1
    end
    print_int(acc)
end

fn is_pal(i: int) -> bool
    a := i
    b := 0
    while(a > 0)
        set b = (10 * b)
        set b = b + (a % 10)
        set a = a/10
    end
    i == b
end
