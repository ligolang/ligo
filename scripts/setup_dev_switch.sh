opam switch create . ocaml-base-compiler.4.06.1
eval $(opam env)
opam install -y ocp-indent tuareg merlin alcotest-lwt crowbar
opam -y user-setup install
