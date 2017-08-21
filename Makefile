all:	server
	@true

deps:
	cd backend && mix deps.get

server:
	cd backend && PORT=3000 mix run --no-halt

shell:
	cd backend && PORT=3000 iex -S mix
