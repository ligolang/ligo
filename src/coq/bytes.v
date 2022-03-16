Require Extraction.

(* Bytes which can be extracted to OCaml *)
Axiom bytes : Set.
Extract Inlined Constant bytes => "bytes".
