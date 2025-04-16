:i count 2
:b shell 59
cargo run -q --bin chs -- compile euler/problem01.chs -r -s
:i returncode 0
:b stdout 7
233168

:b stderr 0

:b shell 18
rm euler/problem01
:i returncode 0
:b stdout 0

:b stderr 0

