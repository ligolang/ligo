open Trace
open Types
include Stage_common.Misc

module Errors = struct
  let different_kinds a b () =
    let title = (thunk "different kinds") in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.type_value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.type_value b )
    ] in
    error ~data title message ()

  let different_constants a b () =
    let title = (thunk "different type constructors") in
    let message () = "Expected these two constant type constructors to be the same, but they're different" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" Stage_common.PP.type_constant a) ;
      ("b" , fun () -> Format.asprintf "%a" Stage_common.PP.type_constant b )
    ] in
    error ~data title message ()
  let different_operators a b () =
    let title = (thunk "different type constructors") in
    let message () = "Expected these two n-ary type constructors to be the same, but they're different" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" (Stage_common.PP.type_operator PP.type_value) a) ;
      ("b" , fun () -> Format.asprintf "%a" (Stage_common.PP.type_operator PP.type_value) b)
    ] in
    error ~data title message ()

  let different_operator_number_of_arguments opa opb lena lenb () =
    let title = (thunk "different number of arguments to type constructors") in
    assert (String.equal (type_operator_name opa) (type_operator_name opb));
    let message () = Format.asprintf
                       "Expected these two n-ary type constructors to be the same, but they have different numbers of arguments (both use the %s type constructor, but they have %d and %d arguments, respectively)"
                       (type_operator_name opa) lena lenb in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" (Stage_common.PP.type_operator PP.type_value) opa) ;
      ("b" , fun () -> Format.asprintf "%a" (Stage_common.PP.type_operator PP.type_value) opb) ;
      ("op" , fun () -> type_operator_name opa) ;
      ("len_a" , fun () -> Format.asprintf "%d" lena) ;
      ("len_b" , fun () -> Format.asprintf "%d" lenb) ;
    ] in
    error ~data title message ()

  let different_size_type name a b () =
    let title () = name ^ " have different sizes" in
    let message () = "Expected these two types to be the same, but they're different (both are " ^ name ^ ", but with a different number of arguments)" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.type_value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.type_value b )
    ] in
    error ~data title message ()

  let different_props_in_record ka kb () =
    let title () = "different keys in record" in
    let message () = "" in
    let data = [
      ("key_a" , fun () -> Format.asprintf "%s" ka) ;
      ("key_b" , fun () -> Format.asprintf "%s" kb )
    ] in
    error ~data title message ()

  let _different_size_constants = different_size_type "type constructors"

  let different_size_sums = different_size_type "sums"

  let different_size_records = different_size_type "records"

  let different_types name a b () =
    let title () = name ^ " are different" in
    let message () = "Expected these two types to be the same, but they're different" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.type_value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.type_value b )
    ] in
    error ~data title message ()

  let different_literals name a b () =
    let title () = name ^ " are different" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()

  let different_values name a b () =
    let title () = name ^ " are different" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.value b )
    ] in
    error ~data title message ()

  let different_literals_because_different_types name a b () =
    let title () = "literals have different types: " ^ name in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()

  let different_values_because_different_types name a b () =
    let title () = "values have different types: " ^ name in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.value b )
    ] in
    error ~data title message ()

  let error_uncomparable_literals name a b () =
    let title () = name ^ " are not comparable" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.literal a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.literal b )
    ] in
    error ~data title message ()

  let error_uncomparable_values name a b () =
    let title () = name ^ " are not comparable" in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.value b )
    ] in
    error ~data title message ()

  let different_size_values name a b () =
    let title () = name in
    let message () = "" in
    let data = [
      ("a" , fun () -> Format.asprintf "%a" PP.value a) ;
      ("b" , fun () -> Format.asprintf "%a" PP.value b )
    ] in
    error ~data title message ()

  let missing_key_in_record_value k () =
    let title () = "missing keys in one of the records" in
    let message () = "" in
    let data = [
      ("missing_key" , fun () -> Format.asprintf "%s" k)
    ] in
    error ~data title message ()

  let missing_entry_point name =
    let title () = "missing entry point" in
    let content () = "no entry point with the given name" in
    let data = [
      ("name" , fun () -> name) ;
    ] in
    error ~data title content

  let not_functional_main location =
    let title () = "not functional main" in
    let content () = "main should be a function" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title content

end

module Free_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

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
    | E_record m -> unions @@ List.map self @@ LMap.to_list m
    | E_record_accessor (a, _) -> self a
    | E_record_update (r,ups) -> union (self r) @@ unions @@ List.map (fun (_,e) -> self e) ups
    | E_tuple_accessor (a, _) -> self a
    | E_list lst -> unions @@ List.map self lst
    | E_set lst -> unions @@ List.map self lst
    | (E_map m | E_big_map m) -> unions @@ List.map self @@ List.concat @@ List.map (fun (a, b) -> [ a ; b ]) m
    | E_look_up (a , b) -> unions @@ List.map self [ a ; b ]
    | E_matching (a , cs) -> union (self a) (matching_expression b cs)
    | E_sequence (a , b) -> unions @@ List.map self [ a ; b ]
    | E_loop (expr , body) -> unions @@ List.map self [ expr ; body ]
    | E_assign (_ , _ , expr) -> self expr
    | E_let_in { binder; rhs; result; _ } ->
      let b' = union (singleton binder) b in
      union
        (annotated_expression b' result)
        (annotated_expression b rhs)

  and lambda : bindings -> lambda -> bindings = fun b l ->
    let b' = union (singleton l.binder) b in
    annotated_expression b' l.body

  and annotated_expression : bindings -> annotated_expression -> bindings = fun b ae ->
    expression b ae.expression

  and matching_variant_case : type a . (bindings -> a -> bindings) -> bindings -> ((constructor * expression_variable) * a) -> bindings  = fun f b ((_,n),c) ->
    f (union (singleton n) b) c

  and matching : type a . (bindings -> a -> bindings) -> bindings -> (a,'var) matching -> bindings = fun f b m ->
    match m with
    | Match_bool { match_true = t ; match_false = fa } -> union (f b t) (f b fa)
    | Match_list { match_nil = n ; match_cons = (hd, tl, c, _) } -> union (f b n) (f (union (of_list [hd ; tl]) b) c)
    | Match_option { match_none = n ; match_some = (opt, s, _) } -> union (f b n) (f (union (singleton opt) b) s)
    | Match_tuple ((lst , a), _) ->
       f (union (of_list lst) b) a    
    | Match_variant (lst,_) -> unions @@ List.map (matching_variant_case f b) lst

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
  | T_constant ca, T_constant cb -> (
      trace_strong (different_constants ca cb)
      @@ Assert.assert_true (ca = cb)
    )
  | T_constant _, _ -> fail @@ different_kinds a b
  | T_operator opa, T_operator opb -> (
    let%bind (lsta, lstb) = match (opa, opb) with
      | TC_option la, TC_option lb
      | TC_list la, TC_list lb
      | TC_contract la, TC_contract lb
      | TC_set la, TC_set lb -> ok @@ ([la], [lb])
      | TC_map (ka,va), TC_map (kb,vb)
      | TC_big_map (ka,va), TC_big_map (kb,vb) -> ok @@ ([ka;va] ,[kb;vb]) 
      | TC_tuple lsta, TC_tuple lstb -> ok @@ (lsta , lstb) 
      | TC_arrow (froma , toa) , TC_arrow (fromb , tob) -> ok @@ ([froma;toa] , [fromb;tob]) 
      | (TC_option _ | TC_list _ | TC_contract _ | TC_set _ | TC_map _ | TC_big_map _ | TC_tuple _ | TC_arrow _),
        (TC_option _ | TC_list _ | TC_contract _ | TC_set _ | TC_map _ | TC_big_map _ | TC_tuple _ | TC_arrow _) -> fail @@ different_operators opa opb
      in
      if List.length lsta <> List.length lstb then
        fail @@ different_operator_number_of_arguments opa opb (List.length lsta) (List.length lstb)
      else
        trace (different_types "arguments to type operators" a b)
        @@ bind_list_iter (fun (a,b) -> assert_type_value_eq (a,b) )(List.combine lsta lstb)
  )
  | T_operator _, _ -> fail @@ different_kinds a b
  | T_sum sa, T_sum sb -> (
      let sa' = CMap.to_kv_list sa in
      let sb' = CMap.to_kv_list sb in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          Assert.assert_true ~msg:"different keys in sum types"
          @@ (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      let%bind _ =
        trace_strong (different_size_sums a b)
        @@ Assert.assert_list_same_size sa' sb' in
      trace (different_types "sum type" a b) @@
      bind_list_iter aux (List.combine sa' sb')
    )
  | T_sum _, _ -> fail @@ different_kinds a b
  | T_record ra, T_record rb -> (
      let ra' = LMap.to_kv_list ra in
      let rb' = LMap.to_kv_list rb in
      let aux ((ka, va), (kb, vb)) =
        let%bind _ =
          trace (different_types "records" a b) @@
          let Label ka = ka in
          let Label kb = kb in
          trace_strong (different_props_in_record ka kb) @@
          Assert.assert_true (ka = kb) in
        assert_type_value_eq (va, vb)
      in
      let%bind _ =
        trace_strong (different_size_records a b)
        @@ Assert.assert_list_same_size ra' rb' in
      trace (different_types "record type" a b)
      @@ bind_list_iter aux (List.combine ra' rb')

    )
  | T_record _, _ -> fail @@ different_kinds a b
  | T_arrow (param, result), T_arrow (param', result') ->
      let%bind _ = assert_type_value_eq (param, param') in
      let%bind _ = assert_type_value_eq (result, result') in
      ok ()
  | T_arrow _, _ -> fail @@ different_kinds a b
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> fail @@ different_kinds a b

(* No information about what made it fail *)
let type_value_eq ab = Trace.to_bool @@ assert_type_value_eq ab

let assert_literal_eq (a, b : literal * literal) : unit result =
  match (a, b) with
  | Literal_bool a, Literal_bool b when a = b -> ok ()
  | Literal_bool _, Literal_bool _ -> fail @@ different_literals "booleans" a b
  | Literal_bool _, _ -> fail @@ different_literals_because_different_types "bool vs non-bool" a b
  | Literal_int a, Literal_int b when a = b -> ok ()
  | Literal_int _, Literal_int _ -> fail @@ different_literals "different ints" a b
  | Literal_int _, _ -> fail @@ different_literals_because_different_types "int vs non-int" a b
  | Literal_nat a, Literal_nat b when a = b -> ok ()
  | Literal_nat _, Literal_nat _ -> fail @@ different_literals "different nats" a b
  | Literal_nat _, _ -> fail @@ different_literals_because_different_types "nat vs non-nat" a b
  | Literal_timestamp a, Literal_timestamp b when a = b -> ok ()
  | Literal_timestamp _, Literal_timestamp _ -> fail @@ different_literals "different timestamps" a b
  | Literal_timestamp _, _ -> fail @@ different_literals_because_different_types "timestamp vs non-timestamp" a b
  | Literal_mutez a, Literal_mutez b when a = b -> ok ()
  | Literal_mutez _, Literal_mutez _ -> fail @@ different_literals "different tezs" a b
  | Literal_mutez _, _ -> fail @@ different_literals_because_different_types "tez vs non-tez" a b
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> fail @@ different_literals "different strings" a b
  | Literal_string _, _ -> fail @@ different_literals_because_different_types "string vs non-string" a b
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> fail @@ different_literals "different bytes" a b
  | Literal_bytes _, _ -> fail @@ different_literals_because_different_types "bytes vs non-bytes" a b
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> fail @@ different_literals_because_different_types "unit vs non-unit" a b
  | Literal_address a, Literal_address b when a = b -> ok ()
  | Literal_address _, Literal_address _ -> fail @@ different_literals "different addresss" a b
  | Literal_address _, _ -> fail @@ different_literals_because_different_types "address vs non-address" a b
  | Literal_signature a, Literal_signature b when a = b -> ok ()
  | Literal_signature _, Literal_signature _ -> fail @@ different_literals "different signature" a b
  | Literal_signature _, _ -> fail @@ different_literals_because_different_types "signature vs non-signature" a b
  | Literal_key a, Literal_key b when a = b -> ok ()
  | Literal_key _, Literal_key _ -> fail @@ different_literals "different key" a b
  | Literal_key _, _ -> fail @@ different_literals_because_different_types "key vs non-key" a b
  | Literal_key_hash a, Literal_key_hash b when a = b -> ok ()
  | Literal_key_hash _, Literal_key_hash _ -> fail @@ different_literals "different key_hash" a b
  | Literal_key_hash _, _ -> fail @@ different_literals_because_different_types "key_hash vs non-key_hash" a b
  | Literal_chain_id a, Literal_chain_id b when a = b -> ok ()
  | Literal_chain_id _, Literal_chain_id _ -> fail @@ different_literals "different chain_id" a b
  | Literal_chain_id _, _ -> fail @@ different_literals_because_different_types "chain_id vs non-chain_id" a b
  | Literal_operation _, Literal_operation _ -> fail @@ error_uncomparable_literals "can't compare operations" a b
  | Literal_operation _, _ -> fail @@ different_literals_because_different_types "operation vs non-operation" a b


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
        generic_try (different_size_values "constants with different number of elements" a b)
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_constant _, E_constant _ ->
      fail @@ different_values "constants" a b
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
      fail @@ different_values "constructors" a b
  | E_constructor _, _ ->
      fail @@ different_values_because_different_types "constructor vs. non-constructor" a b

  | E_tuple lsta, E_tuple lstb -> (
      let%bind lst =
        generic_try (different_size_values "tuples with different number of elements" a b)
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_list @@ List.map assert_value_eq lst in
      ok ()
    )
  | E_tuple _, _ ->
      fail @@ different_values_because_different_types "tuple vs. non-tuple" a b

  | E_record sma, E_record smb -> (
      let aux (Label k) a b =
        match a, b with
        | Some a, Some b -> Some (assert_value_eq (a, b))
        | _              -> Some (fail @@ missing_key_in_record_value k)
      in
      let%bind _all = bind_lmap @@ LMap.merge aux sma smb in
      ok ()
    )
  | E_record _, _ ->
      fail @@ (different_values_because_different_types "record vs. non-record" a b)

  | (E_map lsta, E_map lstb | E_big_map lsta, E_big_map lstb) -> (
      let%bind lst = generic_try (different_size_values "maps of different lengths" a b)
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
  | (E_map _ | E_big_map _), _ ->
      fail @@ different_values_because_different_types "map vs. non-map" a b

  | E_list lsta, E_list lstb -> (
      let%bind lst =
        generic_try (different_size_values "lists of different lengths" a b)
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_list _, _ ->
      fail @@ different_values_because_different_types "list vs. non-list" a b
  | E_set lsta, E_set lstb -> (
      let%bind lst =
        generic_try (different_size_values "sets of different lengths" a b)
          (fun () -> List.combine lsta lstb) in
      let%bind _all = bind_map_list assert_value_eq lst in
      ok ()
    )
  | E_set _, _ ->
      fail @@ different_values_because_different_types "set vs. non-set" a b
  | (E_literal _, _) | (E_variable _, _) | (E_application _, _)
  | (E_lambda _, _) | (E_let_in _, _) | (E_tuple_accessor _, _)
  | (E_record_update _,_)
  | (E_record_accessor _, _)
  | (E_look_up _, _) | (E_matching _, _)
  | (E_assign _ , _)
  | (E_sequence _, _) | (E_loop _, _)-> fail @@ error_uncomparable_values "can't compare sequences nor loops" a b

let merge_annotation (a:type_value option) (b:type_value option) err : type_value result =
  match a, b with
  | None, None -> fail @@ err
  | Some a, None -> ok a
  | None, Some b -> ok b
  | Some a, Some b ->
      let%bind _ = assert_type_value_eq (a, b) in
      match a.simplified, b.simplified with
      | _, None -> ok a
      | _, Some _ -> ok b

let get_entry (lst : program) (name : string) : annotated_expression result =
  trace_option (Errors.missing_entry_point name) @@
  let aux x =
    let (Declaration_constant (an , _, _)) = Location.unwrap x in
    if (an.name = Var.of_name name)
    then Some an.annotated_expression
    else None
  in
  List.find_map aux lst

let program_environment (program : program) : full_environment =
  let last_declaration = Location.unwrap List.(hd @@ rev program) in
  match last_declaration with
  | Declaration_constant (_ , _, (_ , post_env)) -> post_env
