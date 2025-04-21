:i count 2
:b shell 61
cargo run -q --bin chs -- compile-run tests/hello.chs --force
:i returncode 0
:b stdout 60
[INFO] Running executable...
Hello, World!
Hello, 69 World!

:b stderr 0

:b shell 14
rm tests/hello
:i returncode 0
:b stdout 0

:b stderr 0

