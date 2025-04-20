:i count 2
:b shell 64
cargo run -q --bin chs -- compile-run tests/fizzbuzz.chs --force
:i returncode 0
:b stdout 77
[INFO] Running executable...
1
Fizz
Buzz
Fizz
FizzBuzz
Fizz
7
Fizz
Buzz
Fizz

:b stderr 0

:b shell 17
rm tests/fizzbuzz
:i returncode 0
:b stdout 0

:b stderr 0

