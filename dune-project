(lang dune 3.16)

(name ppx_deriving_data)

(generate_opam_files true)

(source
 (github kubrack/ppx_deriving_data))

(authors "Oleksandr Kubrak")

(maintainers "Oleksandr Kubrak")

(license LICENSE)

(package
 (name ppx_deriving_data)
 (synopsis "ppx deriver that will generate random data from the record type definition")
 (depends ocaml dune ppxlib)
 (tags
  (topics ppx_deriving_data)))
