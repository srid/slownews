all:	client
	@true

client:
	elm make web/SlowNews.elm --output=web/static/elm.js
