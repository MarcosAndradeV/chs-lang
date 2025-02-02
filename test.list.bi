:i count 14
:b shell 45
cargo run -q -- compile -r -s tests/while.chs
:i returncode 0
:b stdout 100
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

:b shell 51
cargo run -q -- compile -r -s tests/arithmetics.chs
:i returncode 0
:b stdout 0

:b stderr 55
ERROR: Return type mismatch. Expect: Void  Actual: Int

:b shell 20
rm tests/arithmetics
:i returncode 1
:b stdout 0

:b stderr 86
rm: não foi possível remover 'tests/arithmetics': Arquivo ou diretório inexistente

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

