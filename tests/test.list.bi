:i count 22
:b shell 47
cargo run -q -- compile -r -s tests/big_sum.chs
:i returncode 0
:b stdout 4
100

:b stderr 0

:b shell 16
rm tests/big_sum
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 46
cargo run -q -- compile -r -s tests/arrays.chs
:i returncode 0
:b stdout 20
0
1
2
3
4
5
6
7
8
9

:b stderr 0

:b shell 15
rm tests/arrays
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 45
cargo run -q -- compile -r -s tests/while.chs
:i returncode 0
:b stdout 120
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
Hello CHS
0
1
2
3
4
5
6
7
8
9

:b stderr 0

:b shell 14
rm tests/while
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 43
cargo run -q -- compile -r -s tests/ifs.chs
:i returncode 0
:b stdout 5
1
10

:b stderr 0

:b shell 12
rm tests/ifs
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 43
cargo run -q -- compile -r -s tests/inc.chs
:i returncode 0
:b stdout 2
2

:b stderr 0

:b shell 12
rm tests/inc
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 49
cargo run -q -- compile -r -s tests/mmap_test.chs
:i returncode 0
:b stdout 3
10

:b stderr 0

:b shell 18
rm tests/mmap_test
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 44
cargo run -q -- compile -r -s tests/char.chs
:i returncode 0
:b stdout 9
10
99
48

:b stderr 0

:b shell 13
rm tests/char
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 47
cargo run -q -- compile -r -s tests/simple2.chs
:i returncode 0
:b stdout 3
20

:b stderr 0

:b shell 16
rm tests/simple2
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 45
cargo run -q -- compile -r -s tests/index.chs
:i returncode 0
:b stdout 9
10
20
30

:b stderr 0

:b shell 14
rm tests/index
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 50
cargo run -q -- compile -r -s tests/arithmetic.chs
:i returncode 0
:b stdout 71
Addition: 30
Subtraction: 10
Multiplication: 200
Division: 3
Modulo: 0

:b stderr 0

:b shell 19
rm tests/arithmetic
:i returncode 0
:b stdout 0

:b stderr 0

:b shell 45
cargo run -q -- compile -r -s tests/hello.chs
:i returncode 0
:b stdout 15
Hello from CHS

:b stderr 0

:b shell 14
rm tests/hello
:i returncode 0
:b stdout 0

:b stderr 0

