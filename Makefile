build:
	cargo build --bin chs

release:
	cargo build --release --bin chs

test: build
	./rere.py replay test.list

record: build
	./rere.py record test.list

chsc: release

help:
	@echo "usage: make $(prog)"
