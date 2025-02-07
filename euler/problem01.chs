fn main()
    acc := 0
    i := 0
    while(i < 1000)
        if(i%3 == 0 || i%5 == 0)
            set acc = acc + i
        end
        set i = i + 1
    end
    print_int(acc)
end
