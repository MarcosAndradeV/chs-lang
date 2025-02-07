chs: release
	cp -v target/release/chs .

build:
	cargo build --bin chs

release:
	cargo build --release --bin chs

test: build
	./rere.py replay tests/test.list

record_test: build
	./rere.py record tests/test.list

update_tests: build
	./rere.py update tests tests/test.list

test_euler: build
	./rere.py replay euler/test.list

record_euler: build
	./rere.py record euler/test.list

update_euler: build
	./rere.py update euler euler/test.list

test_full: test test_euler

help:
	@echo "usage: make chs"
