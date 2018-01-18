all:	build	run
	@true

build:
	nix-build

run:
	./result/ghc/backend/bin/backend
