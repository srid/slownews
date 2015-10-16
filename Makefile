SITES := $(shell jq .env.SITES.value < app.json)

all:	client server
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	SITES=${SITES} PORT=4444 mix run --no-halt

shell:
	SITES=${SITES} PORT=4444 iex -S mix

heroku:
	git push heroku master && heroku logs -t
