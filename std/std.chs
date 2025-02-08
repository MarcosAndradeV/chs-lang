
fn strlen(str: string) -> uint
    s: *char = str
    while(*s != '\0') set s = s + 1 end
    cast(uint)s - cast(uint)str
end

fn puts(s: string)
    _ := syscall(1, 1, s, strlen(s))
end
