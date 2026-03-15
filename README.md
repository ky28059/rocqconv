# rocqconv
Rocq -> Prop converter for `CoverageType`.

### Usage
Install dependencies with
```bash
opam install . --deps-only
```
Then, run the convertor with
```bash
dune exec ./bin/main.exe [path to proof file]
```
(if no path is provided, it will assume the Rocq file is located in `/tmp/query.v`).
