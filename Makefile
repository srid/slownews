# TODO: do this from nix 
CABAL_BUILD_DIR_B:=/home/srid/code/slownews/dist-newstyle/build/x86_64-linux/ghc-8.0.2/backend-0.0.1/c/backend/build/backend/backend
CABAL_BUILD_DIR_F:=./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/frontend-1.0.0.0/c/app/build/app/app.jsexe
OUTPUT_DIR:=dist-makefile

all:	nixbuild	nixrun
	@true

nixbuild:
	nix-build

nixrun:
	./result/ghc/backend/bin/backend

# Compile frontend
# From emacs: SPC p c.
f:
	nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"
	mkdir -p ${OUTPUT_DIR}
	ln -sf `pwd`/${CABAL_BUILD_DIR_F} ${OUTPUT_DIR}/static

# Compile backend
b:
	nix-shell -A shells.ghc --run "cabal new-build backend"
	mkdir -p ${OUTPUT_DIR}
	cp ${CABAL_BUILD_DIR_B} ${OUTPUT_DIR}/

# Run backend (assuming frontend is also built)
r:
	${OUTPUT_DIR}/backend

# Run stylish-haskell over modified files
stylish:
	nix-shell -p haskellPackages.stylish-haskell --run "stylish-haskell -i `git diff --name-only | grep .hs`"
