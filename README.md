# **CHS programing language**

CHS is a statically type, native programming language.

## Development Milestones

- [x] Compiled to native code (x86_64-linux)
- [x] Statically typed
- [x] [Fasm](https://flatassembler.net/) as compiler backend

## Examples

- Hello, World:

```chs
fn main()
  msg := "Hello, World\n"
  syscall(1, 1, msg, len(msg))
end
```

## Quick Start

### Building from source

Install [Rust](https://www.rust-lang.org/) and [fasm](https://flatassembler.net/) make sure it's available in `$PATH`.

```console
$ make release
$ cp -v target/release/chs .
```

### Testing

- TODO

### Usage

- TODO

### Editor Support

- TODO

## Language Reference

What the language supports?

TODO

### Type Checking and Inference

TODO

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
