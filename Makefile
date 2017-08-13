SITES := $(shell jq .env.SITES.value < app.json)

all:	client server
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	SITES=${SITES} PORT=3000 mix run --no-halt

shell:
	SITES=${SITES} PORT=3000 iex -S mix

heroku:
	git push -f heroku master && heroku logs -t
