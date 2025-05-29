:i count 2
:b shell 51
cargo run -q --bin chs -- compile-run tests/ifs.chs
:i returncode 0
:b stdout 57
[INFO] Running executable...
true is true
false is false

:b stderr 0

:b shell 12
rm tests/ifs
:i returncode 0
:b stdout 0

:b stderr 0

