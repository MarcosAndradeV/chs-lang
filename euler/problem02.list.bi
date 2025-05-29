:i count 2
:b shell 59
cargo run -q --bin chs -- compile-run ./euler/problem02.chs
:i returncode 0
:b stdout 45
[INFO] Running executable...
Answer: 4613732

:b stderr 0

:b shell 20
rm ./euler/problem02
:i returncode 0
:b stdout 0

:b stderr 0

