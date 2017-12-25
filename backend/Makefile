all:	build run
	@true

build:
	@stack build

run:
	@stack exec backend

## Builds base image used for `stack image container`
build-base:
	@docker build -t slownews-base -f Dockerfile.base .

## Builds app using stack-native.yaml
build-stack-native: build-base
	@stack --stack-yaml stack-native.yaml build
	@stack --stack-yaml stack-native.yaml image container

## Run container built by `stack image container`
run-stack-native:
	@docker run -p 3000:3000 -it -w /opt/app ${IMAGE_NAME} myapp

## Push the image to Heroku docker registry
deploy-heroku:
	@docker push registry.heroku.com/slownews-srid/web

## Build and deploy to Heroku
deploy:	build-stack-native deploy-heroku
	@echo "Deployed to https://slownews-srid.herokuapp.com"
	@echo "Tailing logs (interrupt at any time)..."
	@heroku logs -n 10 -t --app slownews-srid
