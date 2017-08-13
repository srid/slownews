# SlowNews Frontend

## Development

For faster edit-compile-run cycle it is recommended to use `GHCJSi` as follows.

First, enter the Nix configured shell environment:

```
nix-shell -A env
```

Then, enter the GHCJSi shell:


```
cabal configure --ghcjs
cabal repl
```

GHCJSi starts up a web server. You should immediately connect to it from the web browser via http://localhost:6400/


From the GHCJSi shell--and everytime `Main.hs` changes--recompile and send the new code to the browser:

```
> import Miso.Dev  # Once
> :r
> clearBody >> main
```

The UI in the web browser will now update automatically with the new code. Happy hacking!

## Production

This application can be built for production and run as follows:

```
nix-build
warp -d result/bin/app.jsexe/  # Port 3000
```
