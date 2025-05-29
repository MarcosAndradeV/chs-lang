:i count 2
:b shell 59
cargo run -q --bin chs -- compile-run ./euler/problem01.chs
:i returncode 0
:b stdout 47
[INFO] Running executable...
The sum is: 233168
:b stderr 0

:b shell 20
rm ./euler/problem01
:i returncode 0
:b stdout 0

:b stderr 0

