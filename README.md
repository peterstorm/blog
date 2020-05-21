## Developing the project

You can work on the different sub-projects (`client`, `common` and `server`) using `nix-shell` and `cabal`.

`common`:

```bash
cd common
nix-shell
cabal build
...
exit
```

`client`:

```bash
cd client
nix-shell
cabal build
...
exit
```

After the client has been built, you can make a reference to it in the server. The server will need to know where to find the compiled client in order to serve it:

```bash
cd server
mkdir static
ln -sf ../$(find ../client -name all.js) static/
```

Lastly, building and running the server:

```bash
cd server
nix-shell
cabal build
cabal run server
...
exit
```
