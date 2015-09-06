all:	server client
	@true

server:
	GOBIN=./bin go install ./src/server/

client:
	elm make src/client/SlowNews.elm
