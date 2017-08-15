# SlowNews Frontend

## Development

Enter the Nix shell:

```
nix-shell -A env
```

Enter the GHCJSi shell (automatically launches the web server):

```
make
```

From this point, it is sufficient to run `make build` to do a recompilation.

The generated assets--`./frontend/static`--is expected to be served by the backend application. 

### Quick compilation through GHCJSi

Use `make repl` to enter GHCJSi and do quick compilation for catching any type errors.

```
> :r
```
