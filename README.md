# slownews

A web app that aggregates best news during last week from sites like reddit and Hacker News. Uses **Haskell** for backend and **GHCJS** (via [Reflex](https://github.com/reflex-frp/reflex-platform)) for frontend. 

<img src="./screenshot.png" width="75%"></img>

## Running locally

Compilation is all done by Nix. Build and run the app:

```
make
```

Visit http://localhost:3000/

## Developing

Our workflow is based on reflex-platform's [project development](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md#building-with-cabal).

To start the shells:

```
# Backend
nix-shell -A shells.ghc

# Frontend
nix-shell -A shells.ghcjs
```

