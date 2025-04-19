
extern puts(s: string) -> int;
extern strlen(s: string) -> int;

extern printf(s: string, ...) -> int;

fn print_int(n: int)
    printf("%d\n", n);
end
