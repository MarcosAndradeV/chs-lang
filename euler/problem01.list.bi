:i count 2
:b shell 65
cargo run -q --bin chs -- compile-run euler/problem01.chs --force
:i returncode 0
:b stdout 47
[INFO] Running executable...
The sum is: 233168
:b stderr 0

:b shell 18
rm euler/problem01
:i returncode 0
:b stdout 0

:b stderr 0

