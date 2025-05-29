:i count 2
:b shell 59
cargo run -q --bin chs -- compile-run ./euler/problem04.chs
:i returncode 0
:b stdout 44
[INFO] Running executable...
Answer: 906609

:b stderr 0

:b shell 20
rm ./euler/problem04
:i returncode 0
:b stdout 0

:b stderr 0

