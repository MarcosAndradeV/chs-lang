build:
	cargo build --bin chs

release:
	cargo build --release --bin chs

test: build
	./rere.py replay test.list

chs: release

help:
	@echo "usage: make chs"
