# slownews

A web app that aggregates top news during the last week from sites like reddit and Hacker News. Written in **Haskell** and using **Reflex** (via [Obelisk](https://github.com/obsidiansystems/obelisk)).

<img src="./screenshot.png" width="75%"></img>

## Running locally

Install [obelisk](https://github.com/obsidiansystems/obelisk) and run:

```
ob run
```

Visit http://localhost:3001/

NOTE: The frontend uses a hardcoded backend URL (see `Frontend/App.hs:getBaseUrl`), which needs to be changed when deploying SlowNews elsewhere. This will be addressed in the near future as Obelisk improves.

## Release & deploy

To create a release for running locally, run:

```
nix-build -A exe
```

To run it:

```
(cd result && ./backend)
```

To deploy to a remote (NixOS) server, use `ob deploy` (see Obelisk docs for details).
