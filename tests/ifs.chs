extern fn printf(s: string, ...) -> int

fn main() -> int
    if(true)
        printf("true is true\n");
    end
    if(false)
    else
        printf("false is false\n");
    end
    return 0;
end
