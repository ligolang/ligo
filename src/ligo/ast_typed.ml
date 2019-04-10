module S = Ast_simplified

module SMap = Map.String

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Declaration_constant of named_expression
  (* | Macro_declaration of macro_declaration *)

and annotated_expression = {
  expression: expression ;
  type_annotation: tv ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and tv = type_value
and ae = annotated_expression
and tv_map = type_value type_name_map
and ae_map = annotated_expression name_map

and type_value' =
  | T_tuple of tv list
  | T_sum of tv_map
  | T_record of tv_map
  | T_constant of type_name * tv list
  | T_function of tv * tv

and type_value = {
  type_value' : type_value' ;
  simplified : S.type_expression option ;
}

and named_type_value = {
  type_name: name ;
  type_value : type_value ;
}

and lambda = {
  binder: name ;
  input_type: tv ;
  output_type: tv ;
  result: ae ;
  body: block ;
}

and expression =
  (* Base *)
  | E_literal of literal
  | E_constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_application of ae * ae
  | E_lambda of lambda
  (* Tuple *)
  | E_tuple of ae list
  | E_tuple_accessor of ae * int (* Access n'th tuple's element *)
  (* Sum *)
  | E_constructor of name * ae (* For user defined constructors *)
  (* Record *)
  | E_record of ae_map
  | E_record_accessor of ae * string
  (* Data Structures *)
  | E_map of (ae * ae) list
  | E_look_up of (ae * ae)
  (* Advanced *)
  | E_matching of (ae * matching_expr)

and value = annotated_expression (* todo (for refactoring) *)

and literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_string of string
  | Literal_bytes of bytes

and block = instruction list
and b = block

and instruction =
  | I_assignment of named_expression
  | I_matching of ae * matching_instr
  | I_loop of ae * b
  | I_skip
  | I_fail of ae
  | I_patch of named_type_value * access_path * ae

and access = Ast_simplified.access

and access_path = Ast_simplified.access_path

and 'a matching =
  | Match_bool of {
      match_true : 'a ;
      match_false : 'a ;
    }
  | Match_list of {
      match_nil : 'a ;
      match_cons : name * name * 'a ;
    }
  | Match_option of {
      match_none : 'a ;
      match_some : (name * type_value) * 'a ;
    }
  | Match_tuple of name list * 'a

and matching_instr = b matching

and matching_expr = ae matching

open! Trace


let type_value type_value' simplified = { type_value' ; simplified }
let annotated_expression expression type_annotation = { expression ; type_annotation }
let get_type_annotation x = x.type_annotation

let get_entry (p:program) (entry : string) : annotated_expression result =
  let aux (d:declaration) =
    match d with
    | Declaration_constant {name ; annotated_expression} when entry = name -> Some annotated_expression
    | Declaration_constant _ -> None
  in
  let%bind result =
    trace_option (simple_error "no entry point with given name") @@
    Tezos_utils.List.find_map aux p in
  ok result

let get_functional_entry (p:program) (entry : string) : (lambda * type_value) result =
  let%bind entry = get_entry p entry in
  match entry.expression with
  | E_lambda l -> ok (l, entry.type_annotation)
  | _ -> simple_fail "given entry point is not functional"

module PP = struct
  open Format
  open PP

  let list_sep_d x = list_sep x (const " , ")
  let smap_sep_d x = smap_sep x (const " , ")


  let rec type_value' ppf (tv':type_value') : unit =
    match tv' with
    | T_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d type_value) lst
    | T_sum m -> fprintf ppf "sum[%a]" (smap_sep_d type_value) m
    | T_record m -> fprintf ppf "record[%a]" (smap_sep_d type_value) m
    | T_function (a, b) -> fprintf ppf "%a -> %a" type_value a type_value b
    | T_constant (c, []) -> fprintf ppf "%s" c
    | T_constant (c, n) -> fprintf ppf "%s(%a)" c (list_sep_d type_value) n

  and type_value ppf (tv:type_value) : unit =
    type_value' ppf tv.type_value'

  let rec annotated_expression ppf (ae:annotated_expression) : unit =
    match ae.type_annotation.simplified with
    | Some _ -> fprintf ppf "@[<v>%a:%a@]" expression ae.expression type_value ae.type_annotation
    | _ -> fprintf ppf "@[<v>%a@]" expression ae.expression

  and expression ppf (e:expression) : unit =
    match e with
    | E_literal l -> literal ppf l
    | E_constant (c, lst) -> fprintf ppf "%s(%a)" c (list_sep_d annotated_expression) lst
    | E_constructor (c, lst) -> fprintf ppf "%s(%a)" c annotated_expression lst
    | E_variable a -> fprintf ppf "%s" a
    | E_application (f, arg) -> fprintf ppf "(%a) (%a)" annotated_expression f annotated_expression arg
    | E_lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_value input_type type_value output_type
          block body annotated_expression result
    | E_tuple_accessor (ae, i) -> fprintf ppf "%a.%d" annotated_expression ae i
    | E_record_accessor (ae, s) -> fprintf ppf "%a.%s" annotated_expression ae s
    | E_tuple lst -> fprintf ppf "tuple[@;  @[<v>%a@]@;]" (list_sep annotated_expression (tag ",@;")) lst
    | E_record m -> fprintf ppf "record[%a]" (smap_sep_d annotated_expression) m
    | E_map m -> fprintf ppf "map[@;  @[<v>%a@]@;]" (list_sep assoc_annotated_expression (tag ",@;")) m
    | E_look_up (ds, i) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression i
    | E_matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m

  and value ppf v = annotated_expression ppf v

  and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
    fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

  and literal ppf (l:literal) : unit =
    match l with
    | Literal_unit -> fprintf ppf "unit"
    | Literal_bool b -> fprintf ppf "%b" b
    | Literal_int n -> fprintf ppf "%d" n
    | Literal_nat n -> fprintf ppf "%d" n
    | Literal_string s -> fprintf ppf "%s" s
    | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

  and block ppf (b:block) = (list_sep instruction (tag "@;")) ppf b

  and single_record_patch ppf ((s, ae) : string * ae) =
    fprintf ppf "%s <- %a" s annotated_expression ae

  and matching : type a . (formatter -> a -> unit) -> _ -> a matching -> unit = fun f ppf m -> match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep_d (fun ppf -> fprintf ppf "%s")) lst f b
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" f match_nil hd tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" f match_none (fst some) f match_some

  and pre_access ppf (a:access) = match a with
    | Access_record n -> fprintf ppf ".%s" n
    | Access_tuple i -> fprintf ppf ".%d" i

  and instruction ppf (i:instruction) = match i with
    | I_skip -> fprintf ppf "skip"
    | I_fail ae -> fprintf ppf "fail with (%a)" annotated_expression ae
    | I_loop (cond, b) -> fprintf ppf "while (%a) {@;  @[<v>%a@]@;}" annotated_expression cond block b
    | I_assignment {name;annotated_expression = ae} ->
        fprintf ppf "%s := %a" name annotated_expression ae
    | I_matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae (matching block) m
    | I_patch (s, p, e) ->
        fprintf ppf "%s%a := %a"
          s.type_name (fun ppf -> List.iter (pre_access ppf)) p
          annotated_expression e

  let declaration ppf (d:declaration) =
    match d with
    | Declaration_constant {name ; annotated_expression = ae} ->
        fprintf ppf "const %s = %a" name annotated_expression ae

  let program ppf (p:program) =
    fprintf ppf "@[<v>%a@]" (list_sep declaration (tag "@;")) p

end


module Errors = struct
  let different_kinds a b =
    let title = "different kinds" in
    let full = Format.asprintf "(%a) VS (%a)" PP.type_value a PP.type_value b in
    error title full

  let different_constants a b =
    let title = "different constants" in
    let full = Format.asprintf "%s VS %s" a b in
    error title full

  let different_size_type name a b =
    let title = name ^ " have different sizes" in
    let full = Format.asprintf "%a VS %a" PP.type_value a PP.type_value b in
    error title full

  let different_size_constants = different_size_type "constants"

  let different_size_tuples = different_size_type "tuples"

  let different_size_sums = different_size_type "sums"

  let different_size_records = different_size_type "records"

end
open Errors

let rec assert_type_value_eq (a, b: (type_value * type_value)) : unit result = match (a.type_value', b.type_value') with
  | T_tuple ta, T_tuple tb -> (
      let%bind _ =
        trace_strong (different_size_tuples a b)
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
          Assert.assert_true ~msg:"different keys in record types"
          @@ (ka = kb) in
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
let type_value_eq ab = match assert_type_value_eq ab with
  | Ok _ -> true
  | _ -> false

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
  | Literal_string a, Literal_string b when a = b -> ok ()
  | Literal_string _, Literal_string _ -> simple_fail "different strings"
  | Literal_string _, _ -> simple_fail "string vs non-string"
  | Literal_bytes a, Literal_bytes b when a = b -> ok ()
  | Literal_bytes _, Literal_bytes _ -> simple_fail "different bytess"
  | Literal_bytes _, _ -> simple_fail "bytes vs non-bytes"
  | Literal_unit, Literal_unit -> ok ()
  | Literal_unit, _ -> simple_fail "unit vs non-unit"


let rec assert_value_eq (a, b: (value*value)) : unit result =
  let error_content =
    Format.asprintf "%a vs %a" PP.value a PP.value b
  in
  trace (error "not equal" error_content) @@
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
      let error_content =
        Format.asprintf "%a vs %a"
          PP.annotated_expression a
          PP.annotated_expression b
      in
      fail @@ error "comparing constant with other stuff" error_content

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

  | _, _ -> simple_fail "comparing not a value"

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

module Combinators = struct
  let t_bool ?s () : type_value = type_value (T_constant ("bool", [])) s
  let t_string ?s () : type_value = type_value (T_constant ("string", [])) s
  let t_bytes ?s () : type_value = type_value (T_constant ("bytes", [])) s
  let t_int ?s () : type_value = type_value (T_constant ("int", [])) s
  let t_nat ?s () : type_value = type_value (T_constant ("nat", [])) s
  let t_unit ?s () : type_value = type_value (T_constant ("unit", [])) s
  let t_option o ?s () : type_value = type_value (T_constant ("option", [o])) s
  let t_tuple lst ?s () : type_value = type_value (T_tuple lst) s
  let t_pair a b ?s () = t_tuple [a ; b] ?s ()

  let t_record m ?s () : type_value = type_value (T_record m) s
  let make_t_ez_record (lst:(string * type_value) list) : type_value =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    type_value (T_record map) None
  let ez_t_record lst ?s () : type_value =
    let m = SMap.of_list lst in
    t_record m ?s ()

  let t_map key value ?s () = type_value (T_constant ("map", [key ; value])) s

  let t_sum m ?s () : type_value = type_value (T_sum m) s
  let make_t_ez_sum (lst:(string * type_value) list) : type_value =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    type_value (T_sum map) None

  let t_function param result ?s () : type_value = type_value (T_function (param, result)) s

  let get_annotation (x:annotated_expression) = x.type_annotation

  let get_t_bool (t:type_value) : unit result = match t.type_value' with
    | T_constant ("bool", []) -> ok ()
    | _ -> simple_fail "not a bool"

  let get_t_option (t:type_value) : type_value result = match t.type_value' with
    | T_constant ("option", [o]) -> ok o
    | _ -> simple_fail "not a option"

  let get_t_list (t:type_value) : type_value result = match t.type_value' with
    | T_constant ("list", [o]) -> ok o
    | _ -> simple_fail "not a list"

  let get_t_tuple (t:type_value) : type_value list result = match t.type_value' with
    | T_tuple lst -> ok lst
    | _ -> simple_fail "not a tuple"

  let get_t_sum (t:type_value) : type_value SMap.t result = match t.type_value' with
    | T_sum m -> ok m
    | _ -> simple_fail "not a sum"

  let get_t_record (t:type_value) : type_value SMap.t result = match t.type_value' with
    | T_record m -> ok m
    | _ -> simple_fail "not a record"

  let get_t_map (t:type_value) : (type_value * type_value) result =
    match t.type_value' with
    | T_constant ("map", [k;v]) -> ok (k, v)
    | _ -> simple_fail "not a map"
  let assert_t_map (t:type_value) : unit result =
    match t.type_value' with
    | T_constant ("map", [_ ; _]) -> ok ()
    | _ -> simple_fail "not a map"

  let assert_t_int : type_value -> unit result = fun t -> match t.type_value' with
    | T_constant ("int", []) -> ok ()
    | _ -> simple_fail "not an int"

  let assert_t_nat : type_value -> unit result = fun t -> match t.type_value' with
    | T_constant ("nat", []) -> ok ()
    | _ -> simple_fail "not an nat"

  let e_record map : expression = E_record map
  let ez_e_record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    e_record map
  let e_some s : expression = E_constant ("SOME", [s])
  let e_none : expression = E_constant ("NONE", [])

  let e_map lst : expression = E_map lst

  let e_unit : expression = E_literal (Literal_unit)
  let e_int n : expression = E_literal (Literal_int n)
  let e_nat n : expression = E_literal (Literal_nat n)
  let e_bool b : expression = E_literal (Literal_bool b)
  let e_string s : expression = E_literal (Literal_string s)
  let e_pair a b : expression = E_tuple [a; b]

  let e_a_unit = annotated_expression e_unit (t_unit ())
  let e_a_int n = annotated_expression (e_int n) (t_int ())
  let e_a_nat n = annotated_expression (e_nat n) (t_nat ())
  let e_a_bool b = annotated_expression (e_bool b) (t_bool ())
  let e_a_string s = annotated_expression (e_string s) (t_string ())
  let e_a_pair a b = annotated_expression (e_pair a b) (t_pair a.type_annotation b.type_annotation ())
  let e_a_some s = annotated_expression (e_some s) (t_option s.type_annotation ())
  let e_a_none t = annotated_expression e_none (t_option t ())
  let e_a_tuple lst = annotated_expression (E_tuple lst) (t_tuple (List.map get_type_annotation lst) ())
  let e_a_record r = annotated_expression (e_record r) (t_record (SMap.map get_type_annotation r) ())
  let ez_e_a_record r = annotated_expression (ez_e_record r) (ez_t_record (List.map (fun (x, y) -> x, y.type_annotation) r) ())
  let e_a_map lst k v = annotated_expression (e_map lst) (t_map k v ())

  let get_a_int (t:annotated_expression) =
    match t.expression with
    | E_literal (Literal_int n) -> ok n
    | _ -> simple_fail "not an int"

  let get_a_unit (t:annotated_expression) =
    match t.expression with
    | E_literal (Literal_unit) -> ok ()
    | _ -> simple_fail "not a unit"

  let get_a_bool (t:annotated_expression) =
    match t.expression with
    | E_literal (Literal_bool b) -> ok b
    | _ -> simple_fail "not a bool"
end
