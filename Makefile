all:
	elm make SlowNews.elm

fetch:
	mkdir -p data
	./fetchnews.sh ./data
