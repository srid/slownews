all:	client server
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js

server:
	SITES=hn:r/programming:r/todayilearned PORT=4444 mix run --no-halt

shell:
	SITES=hn:r/programming:r/todayilearned PORT=4444 iex -S mix

heroku:
	git push heroku master && heroku logs -t
