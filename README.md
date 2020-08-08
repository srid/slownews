# slownews

A web app that aggregates top news during the last week from sites like reddit, Lobste.rs and Hacker News. Written in **Haskell** and using **Reflex** FRP (via [Obelisk](https://github.com/obsidiansystems/obelisk)).

<img src="./screenshot.png" width="75%"></img>

## Running locally

Install [obelisk](https://github.com/obsidiansystems/obelisk) and run:

```
ob run
```

Visit http://localhost:8000/

## Deploying

To deploy to a remote (NixOS) server, use `ob deploy` (see Obelisk docs for details).

To create a release for running locally, run:

```
nix-build -A exe
```

and then to run that:

```
(cd result && ./backend -p 3001)
```

