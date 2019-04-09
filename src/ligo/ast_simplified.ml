module SMap = Map.String

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Declaration_type of named_type_expression
  | Declaration_constant of named_expression
  (* | Macro_declaration of macro_declaration *)

and annotated_expression = {
  expression: expression ;
  type_annotation: te option ;
}

and named_expression = {
  name: name ;
  annotated_expression: ae ;
}

and named_type_expression = {
  type_name: type_name ;
  type_expression: type_expression ;
}

and te = type_expression
and ae = annotated_expression
and te_map = type_expression type_name_map
and ae_map = annotated_expression name_map

and type_expression =
  | T_tuple of te list
  | T_sum of te_map
  | T_record of te_map
  | T_function of te * te
  | T_variable of type_name
  | T_constant of type_name * te list

and lambda = {
  binder: name ;
  input_type: type_expression ;
  output_type: type_expression ;
  result: ae ;
  body: block ;
}

and expression =
  (* Base *)
  | E_literal of literal
  | E_constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of name
  | E_lambda of lambda
  | E_application of ae * ae
  (* E_Tuple *)
  | E_tuple of ae list
  (* Sum *)
  | E_constructor of name * ae (* For user defined constructors *)
  (* E_record *)
  | E_record of ae_map
  | E_accessor of ae * access_path
  (* Data Structures *)
  | E_map of (ae * ae) list
  | E_look_up of (ae * ae)
  (* Matching *)
  | E_matching of (ae * matching_expr)

and access =
  | Access_tuple of int
  | Access_record of string

and access_path = access list

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
  | I_record_patch of name * access_path * (string * ae) list

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
      match_some : name * 'a ;
    }
  | Match_tuple of name list * 'a

and matching_instr = b matching

and matching_expr = annotated_expression matching

let ae expression = {expression ; type_annotation = None}

let annotated_expression expression type_annotation = {expression ; type_annotation}

open Trace

module PP = struct
  open PP
  open Format

  let list_sep_d x = list_sep x (const " , ")
  let smap_sep_d x = smap_sep x (const " , ")

  let rec type_expression ppf (te:type_expression) = match te with
    | T_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d type_expression) lst
    | T_sum m -> fprintf ppf "sum[%a]" (smap_sep_d type_expression) m
    | T_record m -> fprintf ppf "record[%a]" (smap_sep_d type_expression) m
    | T_function (p, r) -> fprintf ppf "%a -> %a" type_expression p type_expression r
    | T_variable name -> fprintf ppf "%s" name
    | T_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep_d type_expression) lst

  let literal ppf (l:literal) = match l with
    | Literal_unit -> fprintf ppf "Unit"
    | Literal_bool b -> fprintf ppf "%b" b
    | Literal_int n -> fprintf ppf "%d" n
    | Literal_nat n -> fprintf ppf "%d" n
    | Literal_string s -> fprintf ppf "%S" s
    | Literal_bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

  let rec expression ppf (e:expression) = match e with
    | E_literal l -> literal ppf l
    | E_variable name -> fprintf ppf "%s" name
    | E_application (f, arg) -> fprintf ppf "(%a)@(%a)" annotated_expression f annotated_expression arg
    | E_constructor (name, ae) -> fprintf ppf "%s(%a)" name annotated_expression ae
    | E_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep_d annotated_expression) lst
    | E_tuple lst -> fprintf ppf "tuple[%a]" (list_sep_d annotated_expression) lst
    | E_accessor (ae, p) -> fprintf ppf "%a.%a" annotated_expression ae access_path p
    | E_record m -> fprintf ppf "record[%a]" (smap_sep_d annotated_expression) m
    | E_map m -> fprintf ppf "map[%a]" (list_sep_d assoc_annotated_expression) m
    | E_look_up (ds, ind) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression ind
    | E_lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_expression input_type type_expression output_type
          block body annotated_expression result
    | E_matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae (matching annotated_expression) m

  and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
    fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

  and access ppf (a:access) =
    match a with
    | Access_tuple n -> fprintf ppf "%d" n
    | Access_record s -> fprintf ppf "%s" s

  and access_path ppf (p:access_path) =
    fprintf ppf "%a" (list_sep access (const ".")) p

  and type_annotation ppf (ta:type_expression option) = match ta with
    | None -> fprintf ppf ""
    | Some t -> type_expression ppf t

  and annotated_expression ppf (ae:annotated_expression) = match ae.type_annotation with
    | None -> fprintf ppf "%a" expression ae.expression
    | Some t -> fprintf ppf "(%a) : %a" expression ae.expression type_expression t

  and block ppf (b:block) = (list_sep_d instruction) ppf b

  and single_record_patch ppf ((p, ae) : string * ae) =
    fprintf ppf "%s <- %a" p annotated_expression ae

  and matching : type a . (formatter -> a -> unit) -> formatter -> a matching -> unit =
    fun f ppf m -> match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep_d string) lst f b
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" f match_true f match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" f match_nil hd tl f match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" f match_none some f match_some

  and instruction ppf (i:instruction) = match i with
    | I_skip -> fprintf ppf "skip"
    | I_fail ae -> fprintf ppf "fail with (%a)" annotated_expression ae
    | I_record_patch (name, path, lst) -> fprintf ppf "%s.%a[%a]" name access_path path (list_sep_d single_record_patch) lst
    | I_loop (cond, b) -> fprintf ppf "while (%a) { %a }" annotated_expression cond block b
    | I_assignment {name;annotated_expression = ae} ->
        fprintf ppf "%s := %a" name annotated_expression ae
    | I_matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae (matching block) m

  let declaration ppf (d:declaration) = match d with
    | Declaration_type {type_name ; type_expression = te} ->
        fprintf ppf "type %s = %a" type_name type_expression te
    | Declaration_constant {name ; annotated_expression = ae} ->
        fprintf ppf "const %s = %a" name annotated_expression ae

  let program ppf (p:program) =
    fprintf ppf "%a" (list_sep_d declaration) p
end

module Rename = struct
  module Type = struct
    (* Type renaming, not needed. Yet. *)
  end

  module Value = struct
    type renaming = string * (string * access_path) (* src -> dst *)
    type renamings = renaming list
    let filter (r:renamings) (s:string) : renamings =
      List.filter (fun (x, _) -> not (x = s)) r
    let filters (r:renamings) (ss:string list) : renamings =
      List.filter (fun (x, _) -> not (List.mem x ss)) r

    let rec rename_instruction (r:renamings) (i:instruction) : instruction result =
      match i with
      | I_assignment ({name;annotated_expression = e} as a) -> (
          match List.assoc_opt name r with
          | None ->
              let%bind annotated_expression = rename_annotated_expression (filter r name) e in
              ok (I_assignment {a with annotated_expression})
          | Some (name', lst) -> (
              let%bind annotated_expression = rename_annotated_expression r e in
              match lst with
              | [] -> ok (I_assignment {name = name' ; annotated_expression})
              | lst ->
                  let (hds, tl) =
                    let open List in
                    let r = rev lst in
                    rev @@ tl r, hd r
                  in
                  let%bind tl' = match tl with
                    | Access_record n -> ok n
                    | Access_tuple _ -> simple_fail "no support for renaming into tuples yet" in
                  ok (I_record_patch (name', hds, [tl', annotated_expression]))
            )
        )
      | I_skip -> ok I_skip
      | I_fail e ->
          let%bind e' = rename_annotated_expression r e in
          ok (I_fail e')
      | I_loop (cond, body) ->
          let%bind cond' = rename_annotated_expression r cond in
          let%bind body' = rename_block r body in
          ok (I_loop (cond', body'))
      | I_matching (ae, m) ->
          let%bind ae' = rename_annotated_expression r ae in
          let%bind m' = rename_matching rename_block r m in
          ok (I_matching (ae', m'))
      | I_record_patch (v, path, lst) ->
          let aux (x, y) =
            let%bind y' = rename_annotated_expression (filter r v) y in
            ok (x, y') in
          let%bind lst' = bind_map_list aux lst in
          match List.assoc_opt v r with
          | None -> (
              ok (I_record_patch (v, path, lst'))
            )
          | Some (v', path') -> (
              ok (I_record_patch (v', path' @ path, lst'))
            )
    and rename_block (r:renamings) (bl:block) : block result =
      bind_map_list (rename_instruction r) bl

    and rename_matching : type a . (renamings -> a -> a result) -> renamings -> a matching -> a matching result =
      fun f r m ->
      match m with
      | Match_bool { match_true = mt ; match_false = mf } ->
          let%bind match_true = f r mt in
          let%bind match_false = f r mf in
          ok (Match_bool {match_true ; match_false})
      | Match_option { match_none = mn ; match_some = (some, ms) } ->
          let%bind match_none = f r mn in
          let%bind ms' = f (filter r some) ms in
          ok (Match_option {match_none ; match_some = (some, ms')})
      | Match_list { match_nil = mn ; match_cons = (hd, tl, mc) } ->
          let%bind match_nil = f r mn in
          let%bind mc' = f (filters r [hd;tl]) mc in
          ok (Match_list {match_nil ; match_cons = (hd, tl, mc')})
      | Match_tuple (lst, body) ->
          let%bind body' = f (filters r lst) body in
          ok (Match_tuple (lst, body'))

    and rename_matching_instruction = fun x -> rename_matching rename_block x

    and rename_matching_expr = fun x -> rename_matching rename_expression x

    and rename_annotated_expression (r:renamings) (ae:annotated_expression) : annotated_expression result =
      let%bind expression = rename_expression r ae.expression in
      ok {ae with expression}

    and rename_expression : renamings -> expression -> expression result = fun r e ->
      match e with
      | E_literal _ as l -> ok l
      | E_constant (name, lst) ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (E_constant (name, lst'))
      | E_constructor (name, ae) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (E_constructor (name, ae'))
      | E_variable v -> (
          match List.assoc_opt v r with
          | None -> ok (E_variable v)
          | Some (name, path) -> ok (E_accessor (ae (E_variable (name)), path))
        )
      | E_lambda ({binder;body;result} as l) ->
          let r' = filter r binder in
          let%bind body = rename_block r' body in
          let%bind result = rename_annotated_expression r' result in
          ok (E_lambda {l with body ; result})
      | E_application (f, arg) ->
          let%bind f' = rename_annotated_expression r f in
          let%bind arg' = rename_annotated_expression r arg in
          ok (E_application (f', arg'))
      | E_tuple lst ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (E_tuple lst')
      | E_accessor (ae, p) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (E_accessor (ae', p))
      | E_record sm ->
          let%bind sm' = bind_smap
            @@ SMap.map (rename_annotated_expression r) sm in
          ok (E_record sm')
      | E_map m ->
          let%bind m' = bind_map_list
            (fun (x, y) -> bind_map_pair (rename_annotated_expression r) (x, y)) m in
          ok (E_map m')
      | E_look_up m ->
          let%bind m' = bind_map_pair (rename_annotated_expression r) m in
          ok (E_look_up m')
      | E_matching (ae, m) ->
          let%bind ae' = rename_annotated_expression r ae in
          let%bind m' = rename_matching rename_annotated_expression r m in
          ok (E_matching (ae', m'))
  end
end

module Combinators = struct
  let t_bool      : type_expression = T_constant ("bool", [])
  let t_string    : type_expression = T_constant ("string", [])
  let t_bytes     : type_expression = T_constant ("bytes", [])
  let t_int       : type_expression = T_constant ("int", [])
  let t_unit      : type_expression = T_constant ("unit", [])
  let t_option  o : type_expression = T_constant ("option", [o])
  let t_tuple lst : type_expression = T_tuple lst
  let t_pair a b = t_tuple [a ; b]
  let t_record m  : type_expression = (T_record m)
  let t_ez_record (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    T_record map

  let t_record_ez lst =
    let m = SMap.of_list lst in
    t_record m

  let t_sum m : type_expression = T_sum m
  let ez_t_sum (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    T_sum map

  let t_function param result : type_expression = T_function (param, result)

  let e_annotated_expression ?type_annotation expression = {expression ; type_annotation}

  let name (s : string) : name = s

  let e_var (s : string) : expression = E_variable s

  let e_unit  () : expression = E_literal (Literal_unit)
  let e_int n : expression = E_literal (Literal_int n)
  let e_nat n : expression = E_literal (Literal_nat n)
  let e_bool   b : expression = E_literal (Literal_bool b)
  let e_string s : expression = E_literal (Literal_string s)
  let e_bytes  b : expression = E_literal (Literal_bytes (Bytes.of_string b))

  let e_lambda (binder : string)
             (input_type : type_expression)
             (output_type : type_expression)
             (result : expression)
             (body : block)
      : expression =
    E_lambda {
        binder = (name binder) ;
        input_type = input_type ;
        output_type = output_type ;
        result = (ae result) ;
        body ;
      }

  let e_tuple (lst : ae list) : expression = E_tuple lst
  let ez_e_tuple (lst : expression list) : expression =
    e_tuple (List.map (fun e -> ae e) lst)

  let e_constructor (s : string) (e : ae) : expression = E_constructor (name s, e)

  let e_record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    E_record map

  let ez_e_record  (lst : (string * expression) list) : expression =
    (* TODO: define a correct implementation of List.map
     * (an implementation that does not fail with stack overflow) *)
    e_record (List.map (fun (s,e) -> (s, ae e)) lst)
end
