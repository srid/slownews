all:	client
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	SITES=haskell PORT=4444 mix run --no-halt

shell:
	SITES=haskell PORT=4444 iex -S mix
