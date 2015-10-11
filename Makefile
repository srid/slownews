all:	client
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	SITES=hn:programming PORT=4444 mix run --no-halt

shell:
	SITES=hn:programming PORT=4444 iex -S mix

heroku:
	git push heroku master && heroku logs -t
