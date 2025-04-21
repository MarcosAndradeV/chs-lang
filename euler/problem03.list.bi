:i count 2
:b shell 65
cargo run -q --bin chs -- compile-run euler/problem03.chs --force
:i returncode 0
:b stdout 42
[INFO] Running executable...
Answer: 6857

:b stderr 0

:b shell 18
rm euler/problem03
:i returncode 0
:b stdout 0

:b stderr 0

