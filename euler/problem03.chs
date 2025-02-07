fn main()
    fac := 2
    n := 600851475143
    while(n > 1)
        if(n % fac == 0)
            set n = n / fac
        else
            set fac = fac + 1
        end
    end
    print_int(fac)
end
