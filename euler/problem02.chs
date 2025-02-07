fn main()
    a := 1
    b := 2
    acc := 0
    while(a < 4000000)
        if(a%2 == 0) set acc = acc + a end
        c := b
        set b = a + b
        set a = c
    end
    print_int(acc)
end
