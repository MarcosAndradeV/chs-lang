:i count 4
:b shell 49
cargo run -q -- compile -r -s euler/problem01.chs
:i returncode 0
:b stdout 7
233168

:b stderr 0

:b shell 18
rm euler/problem01
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 49
cargo run -q -- compile -r -s euler/problem04.chs
:i returncode 0
:b stdout 7
906609

:b stderr 0

:b shell 18
rm euler/problem04
:i returncode 0
:b stdout 0

:b stderr 0

