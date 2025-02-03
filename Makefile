chs: release
	cp -v target/release/chs .

build:
	cargo build --bin chs

release:
	cargo build --release --bin chs

test: build
	./rere.py replay test.list

help:
	@echo "usage: make chs"
