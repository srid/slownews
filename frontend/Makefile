all:	build repl
	@true

# TODO: new-build is not being used, as the repl is broken. Try after
# upgrading to cabal-install 2.0
build:
	cabal configure -O0 --ghcjs
	cabal build
	cp -v dist-newstyle/build/slownews-frontend-0.1.0.0/build/app/app.jsexe/all.js static/

repl:
	cabal repl
