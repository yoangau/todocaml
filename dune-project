(lang dune 3.16)

(generate_opam_files true)

(source
 (github yoangau/todocaml))

(authors "Yoan Gauthier")

(maintainers "Yoan Gauthier")

(license MIT)

(name todocaml)

(package
 (name todocaml)
 (synopsis "todocaml")
 (description "CLI Todo List App")
 (depends ocaml dune core core_unix base stdio yojson atdgen unix))
