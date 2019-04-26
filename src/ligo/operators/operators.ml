open Trace

module Simplify = struct

  let type_constants = [
    ("unit" , 0) ;
    ("string" , 0) ;
    ("nat" , 0) ;
    ("int" , 0) ;
    ("bool" , 0) ;
    ("operation" , 0) ;
    ("list" , 1) ;
    ("option" , 1) ;
    ("set" , 1) ;
    ("map" , 2) ;
    ("big_map" , 2) ;
  ]

  let constants = [
    ("get_force" , 2) ;
    ("size" , 1) ;
    ("int" , 1) ;
  ]

  module Camligo = struct
    let constants = [
      ("Bytes.pack" , 1) ;
      ("Crypto.hash" , 1) ;
      ("Operation.transfer" , 2) ;
    ]
  end

end

module Typer = struct
  module Errors = struct
    let wrong_param_number = fun name ->
      let title () = "wrong number of params" in
      let full () = name in
      error title full
  end

  open Ast_typed

  type typer_predicate = type_value list -> bool
  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * int * (typer_predicate * typer') list

  let predicate_0 : typer_predicate = fun lst ->
    match lst with
    | [] -> true
    | _ -> false

  let predicate_1 : (type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ] -> f a
    | _ -> false

  let predicate_2 : (type_value -> type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ; b ] -> f a b
    | _ -> false

  let predicate_3 : (type_value -> type_value -> type_value -> bool) -> typer_predicate = fun f lst ->
    match lst with
    | [ a ; b ; c ] -> f a b c
    | _ -> false

  let true_1 = predicate_1 (fun _ -> true)
  let true_2 = predicate_2 (fun _ _ -> true)
  let true_3 = predicate_3 (fun _ _ _ -> true)

  let eq_2 : type_value -> typer_predicate = fun v ->
    let aux = fun a b -> type_value_eq (a, v) && type_value_eq (b, v) in
    predicate_2 aux

  let typer'_0 : (type_value option -> type_result result) -> typer' = fun f lst tv ->
    match lst with
    | [] -> f tv
    | _ -> simple_fail "!!!"

  let typer'_1 : (type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ] -> f a
    | _ -> simple_fail "!!!"

  let typer'_2 : (type_value -> type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ; b ] -> f a b
    | _ -> simple_fail "!!!"

  let typer'_3 : (type_value -> type_value -> type_value -> type_result result) -> typer' = fun f lst _ ->
    match lst with
    | [ a ; b ; c ] -> f a b c
    | _ -> simple_fail "!!!"

  let constant_2 : string -> type_value -> typer' = fun s tv ->
    let aux = fun _ _ -> ok (s, tv) in
    typer'_2 aux

  let make_2 : string -> _ list -> typer = fun name pfs ->
    (name , 2 , List.map (Tuple.map_h_2 predicate_2 typer'_2) pfs)

  let same_2 : string -> (string * type_value) list -> typer = fun s lst ->
    let aux (s, tv) = eq_2 tv, constant_2 s tv in
    (s , 2 , List.map aux lst)

  let very_same_2 : string -> type_value -> typer = fun s tv -> same_2 s [s , tv]

  open Combinators

  let comparator : string -> typer = fun s -> s , 2 , [
      (eq_2 (t_int ()), constant_2 s (t_bool ())) ;
      (eq_2 (t_nat ()), constant_2 s (t_bool ())) ;
    ]

  let boolean_operator_2 : string -> typer = fun s -> very_same_2 s (t_bool ())

  let none = "NONE" , 0 , [
      predicate_0 , typer'_0 (fun tv_opt -> match tv_opt with
          | None -> simple_fail "untyped NONE"
          | Some t -> ok ("NONE", t))
    ]

  let sub = "SUB" , 2 , [
      eq_2 (t_int ()) , constant_2 "SUB_INT" (t_int ()) ;
      eq_2 (t_nat ()) , constant_2 "SUB_NAT" (t_int ()) ;
    ]

  let some = "SOME" , 1 , [
      true_1 , typer'_1 (fun s -> ok ("SOME", t_option s ())) ;
    ]

  let map_remove : typer = "MAP_REMOVE" , 2 , [
      (true_2 , typer'_2 (fun k m ->
          let%bind (src, _) = get_t_map m in
          let%bind () = assert_type_value_eq (src, k) in
          ok ("MAP_REMOVE", m)
        ))
    ]

  let map_update : typer = "MAP_UPDATE" , 3 , [
      (true_3 , typer'_3 (fun k v m ->
           let%bind (src, dst) = get_t_map m in
           let%bind () = assert_type_value_eq (src, k) in
           let%bind () = assert_type_value_eq (dst, v) in
           ok ("MAP_UPDATE", m)))
    ]

  let size : typer = "size" , 1 , [
      (true_1, typer'_1 (fun t ->
           let%bind () = bind_or (assert_t_map t, assert_t_list t) in
           ok ("SIZE", t_nat ())))
    ]

  let get_force : typer = "get_force" , 2 , [
      (true_2, typer'_2 (fun i_ty m_ty ->
           let%bind (src, dst) = get_t_map m_ty in
           let%bind _ = assert_type_value_eq (src, i_ty) in
           ok ("GET_FORCE", dst)))
    ]

  let int : typer = "int" , 1 , [
      (true_1, typer'_1 (fun t ->
           let%bind () = assert_t_nat t in
           ok ("INT", t_int ())))
    ]

  let constant_typers =
    let typer_to_kv : typer -> (string * _) = fun (a, b, c) -> (a, (b, c)) in
    Map.String.of_list
    @@ List.map typer_to_kv [
      same_2 "ADD" [
        ("ADD_INT" , t_int ()) ;
        ("ADD_NAT" , t_nat ()) ;
        ("CONCAT" , t_string ()) ;
      ] ;
      same_2 "TIMES" [
        ("TIMES_INT" , t_int ()) ;
        ("TIMES_NAT" , t_nat ()) ;
      ] ;
      sub ;
      none ;
      some ;
      comparator "EQ" ;
      comparator "LT" ;
      comparator "GT" ;
      comparator "LE" ;
      comparator "GE" ;
      boolean_operator_2 "OR" ;
      boolean_operator_2 "AND" ;
      map_remove ;
      map_update ;
      int ;
      size ;
      get_force ;
    ]

end

module Compiler = struct

  module Michelson = Micheline.Michelson
  open Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson

  let simple_constant c = Constant c

  let simple_unary c = Unary c

  let simple_binary c = Binary c

  let simple_ternary c = Ternary c

  let predicates = Map.String.of_list [
    ("ADD_INT" , simple_binary @@ prim I_ADD) ;
    ("ADD_NAT" , simple_binary @@ prim I_ADD) ;
    ("SUB_INT" , simple_binary @@ prim I_SUB) ;
    ("SUB_NAT" , simple_binary @@ prim I_SUB) ;
    ("TIMES_INT" , simple_binary @@ prim I_MUL) ;
    ("TIMES_NAT" , simple_binary @@ prim I_MUL) ;
    ("NEG" , simple_unary @@ prim I_NEG) ;
    ("OR" , simple_binary @@ prim I_OR) ;
    ("AND" , simple_binary @@ prim I_AND) ;
    ("PAIR" , simple_binary @@ prim I_PAIR) ;
    ("CAR" , simple_unary @@ prim I_CAR) ;
    ("CDR" , simple_unary @@ prim I_CDR) ;
    ("EQ" , simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]) ;
    ("LT" , simple_binary @@ seq [prim I_COMPARE ; prim I_LT]) ;
    ("UPDATE" , simple_ternary @@ prim I_UPDATE) ;
    ("SOME" , simple_unary @@ prim I_SOME) ;
    ("GET_FORCE" , simple_binary @@ seq [prim I_GET ; i_assert_some]) ;
    ("GET" , simple_binary @@ prim I_GET) ;
    ("SIZE" , simple_unary @@ prim I_SIZE) ;
    ("INT" , simple_unary @@ prim I_INT) ;
    ("CONS" , simple_binary @@ prim I_CONS) ;
    ( "MAP_UPDATE" , simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE ]) ;
  ]

end
