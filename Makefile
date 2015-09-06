all:	server client
	@true

server:
	go build ./src/server/

client:
	elm make src/client/SlowNews.elm
