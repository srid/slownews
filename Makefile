all:
	elm make SlowNews.elm --output=static/index.html

fetch:
	mkdir -p data
	./fetchnews.sh ./data
