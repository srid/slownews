# slownews

A web app that aggregates best news during last week from sites like reddit and Hacker News. Uses **Haskell** for backend and **GHCJS** (via [Reflex](https://github.com/reflex-frp/reflex-platform)) for frontend.

<img src="./screenshot.png" width="75%"></img>

## Running locally

Do this once:

```
cp backend/config/config.json ~/.config/slownews.json
```

Compilation is all done by Nix. Build and run the app:

```
nix-build release.nix
( cd result; ./slownews-backend )
```

Visit http://localhost:3000/

## Developing

Our workflow is based on reflex-platform's [project development](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md#building-with-cabal).

### Developing backend

Build the backend:

```
bin/build-backend
```

Build the frontend:

```
bin/build-frontend
```

These commands will place the artifacts under the `./dist-makefile` directory. The backend directly serves the static files copied over in that directory.

Run it:

```
bin/run
```

### Developing frontend

Run the backend (for API):

```
bin/run
```

Run frontend ghci:

```
bin/ghci-frontend
```

Start the frontend server (Note: backend is not running):

```
> main  # Starts server on port 3001
```

Edit-compile-run cycle then looks like:

```
> ^C   # kill warp server
> :r   # reload code
> main # start sever again
```

Alternatively run `bin/ghcid-frontend` which automatically runs the frontend server.
