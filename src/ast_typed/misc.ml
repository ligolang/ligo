open Trace
open Types

module Errors = struct
  let different_kinds a b () =
    let title = (thunk "different kinds") in
    let full () = Format.asprintf "(%a) VS (%a)" PP.type_value a PP.type_value b in
    error title full ()

  let different_constants a b () =
    let title = (thunk "different constants") in
    let full () = Format.asprintf "%s VS %s" a b in
    error title full ()

  let different_size_type name a b () =
    let title () = name ^ " have different sizes" in
    let full () = Format.asprintf "%a VS %a" PP.type_value a PP.type_value b in
    error title full ()

  let different_size_constants = different_size_type "constants"

  let different_size_tuples = different_size_type "tuples"

  let different_size_sums = different_size_type "sums"

  let different_size_records = different_size_type "records"

end

module Free_variables = struct

  type bindings = string list
  let mem : string -> bindings -> bool = List.mem
  let singleton : string -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : string list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings = fun b e ->
    let self = annotated_expression b in
    match e with
    | E_lambda l -> lambda b l
    | E_literal _ -> empty
    | E_constant (_ , lst) -> unions @@ List.map self lst
    | E_variable name -> (
        match mem name b with
        | true -> empty
        | false -> singleton name
      )
    | E_application (a, b) -> unions @@ List.map self [ a ; b ]
    | E_tuple lst -> unions @@ List.map self lst
    | E_constructor (_ , a) -> self a
    | E_record m -> unions @@ List.map self @@ Map.String.to_list m
    | E_record_accessor (a, _) -> self a
    | E_tuple_accessor (a, _) -> self a
    | E_list lst -> unions @@ List.map self lst
    | E_map m -> unions @@ List.map self @@ List.concat @@ List.map (fun (a, b) -> [ a ; b ]) m
    | E_look_up (a , b) -> unions @@ List.map self [ a ; b ]
    | E_matching (a , cs) -> union (self a) (matching_expression b cs)
    | E_failwith a -> self a
    | E_sequence (a , b) -> unions @@ List.map self [ a ; b ]
    | E_loop (expr , body) -> unions @@ List.map self [ expr ; body ]
    | E_assign (_ , _ , expr) -> self expr
    | E_let_in { binder; rhs; result } ->
      let b' = union (singleton binder) b in
      union
        (annotated_expression b' result)
        (annotated_expression b rhs)

  and lambda : bindings -> lambda -> bindings = fun b l ->
    let b' = union (singleton l.binder) b in
    annotated_expression b' l.result

  and annotated_expression : bindings -> annotated_expression -> bindings = fun b ae ->
    expression b ae.expression

  and matching_variant_case : type a . (bindings -> a -> bindings) -> bindings -> ((constructor_name * name) * a) -> bindings  = fun f b ((_,n),c) ->
    f (union (singleton n) b) c

  and matching : type a . (bindings -> a -> bindings) -> bindings -> a matching -> bindings = fun f b m ->
    match m with
    | Match_bool { match_true = t ; match_false = fa } -> union (f b t) (f b fa)
    | Match_list { match_nil = n ; match_cons = (hd, tl, c) } -> union (f b n) (f (union (of_list [hd ; tl]) b) c)
    | Match_option { match_none = n ; match_some = ((opt, _), s) } -> union (f b n) (f (union (singleton opt) b) s)
    | Match_tuple (lst , a) -> f (union (of_list lst) b) a
    | Match_variant (lst , _) -> unions @@ List.map (matching_variant_case f b) lst

  and matching_expression = fun x -> matching annotated_expression x

end


(* module Dependencies = struct
 * 
 *   type bindings = string list
 *   let mem : string -> bindings -> bool = List.mem
 *   let singleton : string -> bindings = fun s -> [ s ]
 *   let union : bindings -> bindings -> bindings = (@)
 *   let unions : bindings list -> bindings = List.concat
 *   let empty : bindings = []
 *   let of_list : string list -> bindings = fun x -> x
 * 
 *   let rec expression : bindings -> full_environment -> expression -> bindings = fun b _env e ->
 *     let self = annotated_expression b in
 *     match e with
 *     | E_lambda l ->
 *         let b' = union (singleton l.binder) b in
 *         let (b'', frees) = block' b' l.body in
 *         union (annotated_expression b'' l.result) frees
 *     | E_literal _ -> empty
 *     | E_constant (_ , lst) -> unions @@ List.map self lst
 *     | E_variable name -> (
 *         match mem name b with
 *         | true -> empty
 *         | false -> singleton name
 *       )
 *     | E_application (a, b) -> unions @@ List.map self [ a ; b ]
 *     | E_tuple lst -> unions @@ List.map self lst
 *     | E_constructor (_ , a) -> self a
 *     | E_record m -> unions @@ List.map self @@ Map.String.to_list m
 *     | E_record_accessor (a, _) -> self a
 *     | E_tuple_accessor (a, _) -> self a
 *     | E_list lst -> unions @@ List.map self lst
 *     | E_map m -> unions @@ List.map self @@ List.concat @@ List.map (fun (a, b) -> [ a ; b ]) m
 *     | E_look_up (a , b) -> unions @@ List.map self [ a ; b ]
 *     | E_matching (a , cs) -> union (self a) (matching_expression b cs)
 *     | E_failwith a -> self a
 * 
 *   and annotated_expression : bindings -> annotated_expression -> bindings = fun b ae ->
 *     let open Combinators in
 *     expression b (get_environment ae) (get_expression ae)
 * 
 *   and instruction' : bindings -> instruction -> bindings * bindings = fun b i ->
 *     match i with
 *     | I_declaration n -> union (singleton n.name) b , (annotated_expression b n.annotated_expression)
 *     | I_assignment n -> b , (annotated_expression b n.annotated_expression)
 *     | I_skip -> b , empty
 *     | I_do e -> b , annotated_expression b e
 *     | I_loop (a , bl) -> b , union (annotated_expression b a) (block b bl)
 *     | I_patch (_ , _ , a) -> b , annotated_expression b a
 *     | I_matching (a , cs) -> b , union (annotated_expression b a) (matching_block b cs)
 * 
 *   and block' : bindings -> block -> (bindings * bindings) = fun b bl ->
 *     let aux = fun (binds, frees) cur ->
 *       let (binds', frees') = instruction' binds cur in
 *       (binds', union frees frees') in
 *     List.fold_left aux (b , []) bl
 * 
 *   and block : bindings -> block -> bindings = fun b bl ->
 *     let (_ , frees) = block' b bl in
 *     frees
 * 
 *   and matching_variant_case : type a . (bindings -> a -> bindings) -> bindings -> ((constructor_name * name) * a) -> bindings  = fun f b ((_,n),c) ->
 *     f (union (singleton n) b) c
 * 
 *   and matching : type a . (bindings -> a -> bindings) -> bindings -> a matching -> bindings = fun f b m ->
 *     match m with
 *     | Match_bool { match_true = t ; match_false = fa } -> union (f b t) (f b fa)
 *     | Match_list { match_nil = n ; match_cons = (hd, tl, c) } -> union (f b n) (f (union (of_list [hd ; tl]) b) c)
 *     | Match_option { match_none = n ; match_some = ((opt, _), s) } -> union (f b n) (f (union (singleton opt) b) s)
 *     | Match_tuple (lst , a) -> f (union (of_list lst) b) a
 *     | Match_variant (lst , _) -> unions @@ List.map (matching_variant_case f b) lst
 * 
 *   and matching_expression = fun x -> matching annotated_expression x
 * 
 *   and matching_block = fun x -> matching block x
 * 
 * end *)


open Errors

let rec assert_type_value_eq (a, b: (type_value * type_value)) : unit result = match (a.type_value', b.type_value') with
  | T_tuple ta, T_tuple tb -> (
      let%bind _ =
        trace_strong (fun () -> (different_size_tuples a b ()))
        @@ Assert.assert_true List.(length ta = length tb) in
      bind_list_iter assert_type_value_eq (List.combine ta tb)
    )
  | T_tuple _, _ -> fail @@ different_kinds a b
  | T_constant (ca, lsta), T_constant (cb, lstb) -> (
      let%bind _ =
        trace_strong (different_size_constants a b)
        @@ Assert.assert_true List.(length lsta = length lstb) in
      let%bind _ =
        trace_strong (different_constants ca cb)
        @@ Assert.assert_true (ca = cb) in
      trace (simple_error "constant sub-expression")
      @@ bind_list_iter assert_type_value_eq (List.combine lsta lstb)
    )
  | T_constant _, _ -> fail @@ different_kinds a b
  | T_sum sa, T_sum sb -> (
      let sa' = SMap.to_kv_list sa in
      let sb' = SMap.to_kv_list sb in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in sum types"
          @@ (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      let%bind _ =
        trace_strong (different_size_sums a b)
        @@ Assert.assert_list_same_size sa' sb' in
      trace (simple_error "sum type") @@
      bind_list_iter aux (List.combine sa' sb')
    )
  | T_sum _, _ -> fail @@ different_kinds a b
  | T_record ra, T_record rb -> (
      let ra' = SMap.to_kv_list ra in
      let rb' = SMap.to_kv_list rb in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          let error =
            let title () = "different props in record" in
            let content () = Format.asprintf "%s vs %s" ka kb in
            error title content in
          trace_strong error @@
          Assert.assert_true (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      let%bind _ =
        trace_strong (different_size_records a b)
        @@ Assert.assert_list_same_size ra' rb' in
      trace (simple_error "record type")
      @@ bind_list_iter aux (List.combine ra' rb')

    )
  | T_record _, _ -> fail @@ different_kinds a b
  | T_function (param, result), T_function (param', result') ->
      let%bind _ = assert_type_value_eq (param, param') in
      let%bind _ = assert_type_value_eq (result, result') in
      ok ()
  | T_function _, _ -> fail @@ different_kinds a b

(* No information about what made it fail *)
let type_value_eq ab = Trace.to_bool @@ assert_type_value_eq ab

let assert_literal_eq (a, b : literal * literal) : unit result =
  match (a, b) with
  | Literal_bool a, Literal_bool b when a = b -> ok ()
  | Literal_bool _, Literal_bool _ -> simple_fail "different bools"
  | Literal_bool _, _ -> simple_fail "bool vs non-bool"
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> simple_fail "different ints"
  | Literal_int _, _ -> simple_fail "int vs non-int"
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> simple_fail "different nats"
  | Literal_nat _, _ -> simple_fail "nat vs non-nat"
  | Literal_tez a, Literal_tez b when a = b -> ok ()
  | Literal_tez _, Literal_tez _ -> simple_fail "different tezs"
  | Literal_tez _, _ -> simple_fail "tez vs non-tez"
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> simple_fail "different strings"
  | Literal_string _, _ -> simple_fail "string vs non-string"
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> simple_fail "different bytess"
  | Literal_bytes _, _ -> simple_fail "bytes vs non-bytes"
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> simple_fail "unit vs non-unit"
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> simple_fail "different addresss"
  | Literal_address _, _ -> simple_fail "address vs non-address"
  | Literal_operation _, Literal_operation _ -> simple_fail "can't compare operations"
  | Literal_operation _, _ -> simple_fail "operation vs non-operation"


let rec assert_value_eq (a, b: (value*value)) : unit result =
  let error_content () =
    Format.asprintf "\n%a vs %a" PP.value a PP.value b
  in
  trace (fun () -> error (thunk "not equal") error_content ()) @@
  match (a.expression, b.expression) with
  | E_literal a, E_literal b ->
      assert_literal_eq (a, b)
  | E_constant (ca, lsta), E_constant (cb, lstb) when ca = cb -> (
      let%bind lst =
        generic_try (simple_error "constants with different number of elements")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_constant _, E_constant _ ->
      simple_fail "different constants"
  | E_constant _, _ ->
      let error_content () =
        Format.asprintf "%a vs %a"
          PP.annotated_expression a
          PP.annotated_expression b
      in
      fail @@ (fun () -> error (thunk "comparing constant with other stuff") error_content ())

  | E_constructor (ca, a), E_constructor (cb, b) when ca = cb -> (
      let%bind _eq = assert_value_eq (a, b) in
      ok ()
    )
  | E_constructor _, E_constructor _ ->
      simple_fail "different constructors"
  | E_constructor _, _ ->
      simple_fail "comparing constructor with other stuff"

  | E_tuple lsta, E_tuple lstb -> (
      let%bind lst =
        generic_try (simple_error "tuples with different number of elements")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_tuple _, _ ->
      simple_fail "comparing tuple with other stuff"

  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _ -> Some (simple_fail "different record keys")
      in
      let%bind _all = bind_smap @@ SMap.merge aux sma smb in
      ok ()
    )
  | E_record _, _ ->
      simple_fail "comparing record with other stuff"

  | E_map lsta, E_map lstb -> (
      let%bind lst = generic_try (simple_error "maps of different lengths")
          (fun () ->
             let lsta' = List.sort compare lsta in
             let lstb' = List.sort compare lstb in
             List.combine lsta' lstb') in
      let aux = fun ((ka, va), (kb, vb)) ->
        let%bind _ = assert_value_eq (ka, kb) in
        let%bind _ = assert_value_eq (va, vb) in
        ok () in
      let%bind _all = bind_map_list aux lst in
      ok ()
    )
  | E_map _, _ ->
      simple_fail "comparing map with other stuff"

  | E_list lsta, E_list lstb -> (
      let%bind lst =
        generic_try (simple_error "list of different lengths")
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_list _, _ ->
      simple_fail "comparing list with other stuff"
  | (E_literal _, _) | (E_variable _, _) | (E_application _, _)
  | (E_lambda _, _) | (E_let_in _, _) | (E_tuple_accessor _, _)
  | (E_record_accessor _, _)
  | (E_look_up _, _) | (E_matching _, _) | (E_failwith _, _)
  | (E_assign _ , _)
  | (E_sequence _, _) | (E_loop _, _)-> simple_fail "comparing not a value"

let merge_annotation (a:type_value option) (b:type_value option) : type_value result =
  match a, b with
  | None, None -> simple_fail "no annotation"
  | Some a, None -> ok a
  | None, Some b -> ok b
  | Some a, Some b ->
      let%bind _ = assert_type_value_eq (a, b) in
      match a.simplified, b.simplified with
      | _, None -> ok a
      | _, Some _ -> ok b
