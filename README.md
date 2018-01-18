# slownews

A web app that aggregates best news during last week from sites like reddit and Hacker News. Uses **Haskell** for backend and **GHCJS** (via [Reflex](https://github.com/reflex-frp/reflex-platform)) for frontend. 

<img src="./screenshot.png" width="75%"></img>

## Running locally

Compilation is all done by Nix:

```
make build
```

Run the app:

```
make
```

Visit http://localhost:3000/

