(* This wrapper exists to avoid vendored transitive dependencies in
   the coq extraction, in order to work around a dune/coq bug *)

module M1 = Micheline (* from extraction *)
module M2 = Tezos_micheline.Micheline (* from Tezos dependency *)

let rec forward (x : _ M1.node) : _ M2.node =
  match x with
  | Prim (m, p, args, annot) ->
    M2.Prim (m, p, List.map forward args, annot)
  | Seq (m, args) ->
    M2.Seq (m, List.map forward args)
  | Int (m, x) -> Int (m, x)
  | String (m, x) -> String (m, x)
  | Bytes (m, x) -> Bytes (m, x)

let rec backward (x : _ M2.node) : _ M1.node =
  match x with
  | Prim (m, p, args, annot) ->
    M1.Prim (m, p, List.map backward args, annot)
  | Seq (m, args) ->
    M1.Seq (m, List.map backward args)
  | Int (m, x) -> Int (m, x)
  | String (m, x) -> String (m, x)
  | Bytes (m, x) -> Bytes (m, x)
