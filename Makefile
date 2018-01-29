all:	build	run
	@true

build:
	nix-build

run:
	./result/ghc/backend/bin/backend

# Compile frontend
# From emacs: SPC p c.
f:
	nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"

# Compile backend
b:
	nix-shell -A shells.ghc --run "cabal new-build all"

# Run stylish-haskell over modified files
stylish:
	nix-shell -p haskellPackages.stylish-haskell --run "stylish-haskell -i `git diff --name-only | grep .hs`"
