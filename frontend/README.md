# SlowNews Frontend

## Development

For faster edit-compile-run cycle it is recommended to use `GHCJSi` as follows.

Enter the Haskell shell and automatically run the web server:

```
nix-shell -A env --command "cabal configure --ghcjs && cabal repl"
```

GHCJSi starts up a web server. You should immediately connect to it from the web browser via http://localhost:6400/


From the GHCJSi shell--and everytime `Main.hs` changes--recompile and send the new code to the browser:

```
> import Miso.Dev  # Once
> :r
> clearBody >> main
```

The UI in the web browser will now update automatically with the new code. Happy hacking!

### cabal new-build

As of now (14 Aug, 2017) static files such as HTML and CSS [cannot be served](https://github.com/ghcjs/ghcjs/issues/530) by GHCJSi. Thus, in order to use index.html with CSS references in its `<head>` tag, the following build workflow should preferred to the one above:

```
nix-shell -A env
...
cabal install cabal-install  # Recommended, but optional.
cabal new-configure -O0 --ghcjs
cabal new-build
```

That last command, `cabal new-build` is expected to complete in a reasonably short time (10 seconds, when first tested). Resulting static files can be accessed from `./dist-newstyle/build/slownews-frontend-0.1.0.0/build/app/app.jsexe/`.

## Production

This application can be built for production and run as follows:

```
nix-build
warp -d result/bin/app.jsexe/  # Port 3000
```
