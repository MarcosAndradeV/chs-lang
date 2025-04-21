# CHS Programming Language

CHS is a modern, statically typed, native programming language focused on safety, performance, and developer ergonomics.

## Current Status

The CHS compiler supports essential language features including static typing, type inference, and native code generation for x86_64 Linux. The compiler is actively developed with regular improvements and new features being added.

## Design Goals

- **Safety First:** CHS prioritizes compile-time safety through strong static typing and planned memory management.
- **Developer Experience:** Clean syntax and helpful error messages make development productive and enjoyable.
- **Performance:** Native code generation and zero-cost abstractions ensure optimal runtime performance.
- **Extensibility:** The language serves as a foundation for experimentation and domain-specific extensions.

## Key Features

- **Modern Type System:** Strong static typing with powerful type inference, inspired by ML-family languages and modern type system research.
- **Zero-Cost Abstractions:** High-level programming constructs that compile to efficient machine code.
- **Compile-Time Memory Management (In Progress):** Memory safety enforced at compile-time, similar to Rust's ownership system.
- **Helpful Compiler Messages:** Clear and actionable error messages to aid development.
- **Native Performance:** Direct compilation to x86_64 machine code without a runtime or garbage collector.

## Limitations

- **Early Development:** While core features are implemented, some advanced features are still in development.
- **Platform Support:** Currently targets x86_64 Linux only. Other platforms planned.

## Roadmap

- [x] Native code compiler (x86_64-linux)
- [x] Static type system with type inference
- [x] Basic control flow and function support
- [x] Advanced type system features (Generics, Sum types, Pattern matching)
- [ ] Module system
- [ ] Standard library with core data structures
- [ ] Multi-platform support (Windows, macOS)
- [ ] IDE integration and debugging support
- [ ] Memory management system
- [ ] Documentation generator

## Getting Started

### Prerequisites

- [Rust](https://www.rust-lang.org/) toolchain (2021 edition or later)
- [QBE](https://c9x.me/compile/) backend (must be in `$PATH`)
- Linux x86_64 system

### Building from Source

```bash
git clone https://github.com/yourusername/chs-lang.git
cd chs-lang
cargo build --release
cp target/release/chs .
```

### Testing

```bash
cargo run --bin test_maker -- -h
# Usage: test_maker [OPTIONS] <TASK>
# Options:
#   --test-folder <FOLDER> Set test folder. Default: ./tests
#   --test-file <FILE>     Switch to single file mode
# Tasks:
#   --record, -r           Record tests
#   --replay, -p           Replay tests
#   --write, -w            Write tests
#   --reset                Reset tests
#   --help, -h             Print this help message
```


### Usage:

```bash
./chs
# Compile and run chs files
#
# Usage: chs <COMMAND>
#
# Commands:
#   compile      Compile a source file
#   compile-run  Compile and run the executable
#   help         Print this message or the help of the given subcommand(s)
#
# Options:
#   -h, --help     Print help
#   -V, --version  Print version
```

### Editor Support

Editor support is coming soon! Planned features:
- Syntax highlighting
- Code completion
- Inline type information
- Error highlighting

## Language Reference

### Examples

#### Hello World
```chs
fn main() -> int
    puts("Hello, World!")
    return 0
end
```

#### Basic Types and Functions
```chs
fn add(x: int, y: int) -> int
    return x + y
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