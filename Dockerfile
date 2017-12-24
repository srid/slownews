FROM haskell:8.2

RUN stack --version && stack upgrade && stack --version

RUN apt-get -y update && apt-get install -y xz-utils build-essential make

RUN stack setup

# First cache the long build process
COPY backend/backend.cabal /app/backend/
COPY backend/stack.yaml /app/backend/
WORKDIR /app/backend
RUN stack install --only-dependencies

COPY . /app
RUN stack build

CMD stack exec backend

EXPOSE 3000
