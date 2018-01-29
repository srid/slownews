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

### Developing backend (and frontend)

Build the backend:

```
make b
```

Build the frontend:

```
make f
```

These commands will place the artifacts under the `./dist-makefile` directory. The backend directly serves the static files copied over in that directory.

Run it:

```
make r
```

### Developing frontend (only)

Let's start by entering the GHC environment:

```
nix-shell -A shells.ghc
```

To develop the frontend, launch ghci and start the jsaddle warp server:

```
cabal new-repl frontend
> main  # Starts server on port 3001
```

Edit-compile-run cycle then looks like:

```
> ^C   # kill warp server
> :r   # reload code 
> main # start sever again
```

