:i count 2
:b shell 61
cargo run -q --bin chs -- compile-run tests/while.chs --force
:i returncode 0
:b stdout 89
[INFO] Running executable...
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5
i = 6
i = 7
i = 8
i = 9

:b stderr 0

:b shell 14
rm tests/while
:i returncode 0
:b stdout 0

:b stderr 0

