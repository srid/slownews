JSEXEDIR := Frontend.jsexe

all:	build
	@true

build:
	ghcjs Frontend.hs
	cp -v  ${JSEXEDIR}/all.js ./static/
