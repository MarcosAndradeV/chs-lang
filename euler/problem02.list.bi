:i count 2
:b shell 59
cargo run -q --bin chs -- compile euler/problem02.chs -r -s
:i returncode 0
:b stdout 8
4613732

:b stderr 0

:b shell 18
rm euler/problem02
:i returncode 0
:b stdout 0

:b stderr 0

