all:	client server
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	cd backend && PORT=3000 mix run --no-halt

shell:
	cd backend && PORT=3000 iex -S mix
