:i count 2
:b shell 58
cargo run -q --bin chs -- compile tests/fizzbuzz.chs -r -s
:i returncode 0
:b stdout 35
1
2
4
7
8
Fizz
Fizz
Buzz
Fizz
Fizz

:b stderr 0

:b shell 17
rm tests/fizzbuzz
:i returncode 0
:b stdout 0

:b stderr 0

