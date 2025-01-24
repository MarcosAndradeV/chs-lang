build:
	cargo build --bin chs

release:
	cargo build --release --bin chs

test:
	cargo test --all

chs: release

help:
	@echo "usage: make chs"
