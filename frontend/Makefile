JSEXEDIR := .stack-work//install/x86_64-osx/lts-6.30/ghcjs-0.2.0.9006030_ghc-7.10.3/bin/app.jsexe

all:	build
	@true

build:
	stack build
	cp -v  ${JSEXEDIR}/all.js ./static/

repl:
	stack ghci
