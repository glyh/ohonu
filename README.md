# OHonu - Minimal Implementation of Honu Macros

### Requirements
- opam

### Building

- Clone the repo and init the switch
```sh
git clone https://github.com/glyh/ohonu
cd ohonu
opam switch create . --deps-only --with-test -y
```
- For developing, you may want to have LSP and other stuffs available
```sh
opam install --switch=. -y ocamlformat ocaml-lsp-server utop
```
- Update the environment, for example if you're on bash: 
```bash
eval $(opam env)
```
- Build and run the package
```sh
dune exec ohonu
```

### References

- [Binding as Sets of Scopes](https://www-old.cs.utah.edu/plt/scope-sets/index.html)
