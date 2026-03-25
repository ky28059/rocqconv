# rocqconv
Rocq -> Prop converter for `CoverageType`.

### Usage
Install dependencies with
```bash
opam install . --deps-only
```
Then, run the converter with
```bash
dune exec ./bin/main.exe [path to proof file] [path to output file]
```
(the input file will default to `/tmp/query.v`, and the output file with default to `/tmp/axioms.ml`).
