# slownews

A web app that aggregates best news during last week from sites like reddit and Hacker News. Uses **Haskell** for backend and **GHCJS** (via [Reflex](https://github.com/reflex-frp/reflex-platform)) for frontend.

<img src="./screenshot.png" width="75%"></img>

## Running locally

Install [obelisk](https://github.com/obsidiansystems/obelisk) and run:

```
ob run
```

Visit http://localhost:3001/

NOTE: The frontend uses a hardcoded backend URL (see `Frontend/App.hs:getBaseUrl`), which needs to be changed when deploying SlowNews elsewhere. This will be addressed in the near future as Obelisk improves.

## Release & deploy

TODO: `ob deploy` and `nix-build -A exe`
