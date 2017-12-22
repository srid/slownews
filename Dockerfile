FROM haskell:8.2

RUN stack --version && stack upgrade && stack --version

RUN apt-get -y update && apt-get install -y xz-utils build-essential make

RUN stack setup

ADD . /app
WORKDIR /app/backend
RUN stack build

CMD stack exec backend
