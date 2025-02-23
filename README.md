# CHS Programming Language

CHS is a statically typed, native programming language for experimentation.

## Current Status

The CHS compiler is currently functional, allowing for basic program compilation and execution. However, the language is still under active development.

## Design Goals

- **Simplicity:** CHS aims to be easy to learn and use.
- **Explicitness:** The language promotes clear and explicit programming practices, making code more readable and maintainable.
- **Extensibility:** CHS provides a foundation for experimentation and allows for the development of domain-specific extensions.

## Key Features

- **Statically Typed:** CHS incorporates a robust static type system with type inference, inspired by the ML family of languages, enhancing code safety and reliability.
- **Metaprogramming (Planned):** CHS aims to support metaprogramming capabilities, allowing developers to generate code at compile time, enabling powerful code generation and optimization techniques.
- **Compile-Time Memory Management (Planned):** CHS plans to integrate a compile-time memory management system, similar to Rust, to improve memory safety and performance.

## Limitations

- **Limited Practicality:** The current version lacks a module system and a standard library, which limits its practical usability.
- **Metaprogramming:** The metaprogramming system is currently planned and not yet implemented.

## Roadmap

- [x] Native code compiler (x86_64-linux)
- [x] Statically typed and type inference
- [ ] Module System: Implement a robust module system to support code organization and reusability.
- [ ] Standard Library: Develop a comprehensive standard library providing essential functions and data structures.
- [ ] Metaprogramming System: Implement the planned metaprogramming capabilities, enabling code generation and optimization.

## Getting Started

### Building from Source:

Install [Rust](https://www.rust-lang.org/) and [fasm](https://flatassembler.net/) make sure it's available in `$PATH`.

```console
$ make chs.
```

### Testing:

```console
$ ./rere.py replay test.list
```

### Usage:

```console
$ ./chs
Usage: chs <COMMAND> [ARGS] [[-|--]FLAG]
COMMANDS:
    help                                                    Print help message
    compile <INPUT> [--emit-asm]  [-o <OUTPUT>] [-r]  [-s]  Compiles the program.
    check <INPUT>                                           Check the program compile.
```

### Editor Support:

- No support yet

## Language Reference

### Examples

- Hello, World:
    
```chs
use "std.chs"

fn main()
   puts("Hello from CHS\n")
end
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References & Inspirations

- Type System [GitHub tomprimozic/type-systems](https://github.com/tomprimozic/type-systems)
- BM: [GitHub - tsoding/bm](https://github.com/tsoding/bm)
- Porth: [GitLab - tsoding/porth](https://gitlab.com/tsoding/porth)
- SmallVM: [GitHub - tarekwiz/smallvm](https://github.com/tarekwiz/smallvm)
- IridiumVM: [GitHub - fhaynes/iridium](https://github.com/fhaynes/iridium)
- Inko: [GitHub - inko-lang/inko](https://github.com/inko-lang/inko)
- Boson-lang: [GitHub - Narasimha1997/boson-lang](https://github.com/Narasimha1997/boson-lang)
- Tao: [GitHub - zesterer/tao](https://github.com/zesterer/tao)
