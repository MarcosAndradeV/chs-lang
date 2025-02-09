
fn strlen(str: string) -> uint
    s: *char = str
    while(*s != '\0') set s = s + 1 end
    cast(uint)s - cast(uint)str
end

fn puts(s: string) _ := syscall(1, 1, s, strlen(s)) end

fn fputs(fd: uint, s: string) _ := syscall(1, fd, s, strlen(s)) end

fn memset(data: *void, size: uint, byte: char) -> *void
    data := cast(*char)data
    while(size > cast(uint)0)
        set data[cast(int)size] = byte
        set size = size - cast(uint)1
    end
    data
end
