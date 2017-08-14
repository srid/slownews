# SlowNews Frontend

## Development

For faster edit-compile-run cycle it is recommended to use `GHCJSi` as follows.

Enter the Nix shell:

```
nix-shell -A env
```

Enter the GHCJSi shell (automatically launches the web server):

```
make
```

Go to http://localhost:6400/


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
nix-shell -A env
...
make build
```

Resulting static files can be accessed from `./dist-newstyle/build/slownews-frontend-0.1.0.0/build/app/app.jsexe/`.
