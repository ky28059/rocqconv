(lang dune 3.21)

(name rocqconv)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rocqconv)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (rocq-stdlib (>= 9.0.0))
  (rocq-runtime (>= 9.0.1)))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
