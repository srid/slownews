all:	build repl
	@true

build:
	cabal new-configure -O0 --ghcjs
	cabal new-build
	cp -v dist-newstyle/build/slownews-frontend-0.1.0.0/build/app/app.jsexe/all.js static/

repl:
	cabal new-repl
