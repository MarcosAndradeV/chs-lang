extern fn printf(s: string, ...) -> int

fn main() -> int
    printf("Hello, World!\n");
    printf("Hello, %d World!\n", 69);
    return 0;
end
