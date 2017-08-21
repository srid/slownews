all:	configure build repl
	@true

# TODO: new-build is not being used, as the repl is broken. Try after
# upgrading to cabal-install 2.0
configure:
	cabal configure -O0 --ghcjs

build:
	cabal build
	cp -v  dist/build/app/app.jsexe/all.js ./static/

repl:
	cabal repl
