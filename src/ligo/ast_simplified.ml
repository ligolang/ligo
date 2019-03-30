module SMap = Ligo_helpers.X_map.String

type name = string
type type_name = string

type 'a name_map = 'a SMap.t
type 'a type_name_map = 'a SMap.t

type program = declaration list

and declaration =
  | Type_declaration of named_type_expression
  | Constant_declaration of named_expression
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
  | Type_tuple of te list
  | Type_sum of te_map
  | Type_record of te_map
  | Type_function of te * te
  | Type_variable of type_name
  | Type_constant of type_name * te list

and lambda = {
  binder: name ;
  input_type: type_expression ;
  output_type: type_expression ;
  result: ae ;
  body: block ;
}

and expression =
  (* Base *)
  | Literal of literal
  | Constant of name * ae list (* For language constants, like (Cons hd tl) or (plus i j) *)
  | Variable of name
  | Lambda of lambda
  | Application of ae * ae
  (* Tuple *)
  | Tuple of ae list
  (* Sum *)
  | Constructor of name * ae (* For user defined constructors *)
  (* Record *)
  | Record of ae_map
  | Accessor of ae * access_path
  (* Data Structures *)
  | Map of (ae * ae) list
  | LookUp of (ae * ae)

and access =
  | Tuple_access of int
  | Record_access of string

and access_path = access list

and literal =
  | Unit
  | Bool of bool
  | Number of int
  | String of string
  | Bytes of bytes

and block = instruction list
and b = block

and instruction =
  | Assignment of named_expression
  | Matching of ae * matching
  | Loop of ae * b
  | Skip
  | Fail of ae
  | Record_patch of name * access_path * (string * ae) list

and matching =
  | Match_bool of {
      match_true : b ;
      match_false : b ;
    }
  | Match_list of {
      match_nil : b ;
      match_cons : name * name * b ;
    }
  | Match_option of {
      match_none : b ;
      match_some : name * b ;
    }
  | Match_tuple of name list * b

let ae expression = {expression ; type_annotation = None}

let annotated_expression expression type_annotation = {expression ; type_annotation}

open Ligo_helpers.Trace

module PP = struct
  open Ligo_helpers.PP
  open Format

  let rec type_expression ppf (te:type_expression) = match te with
    | Type_tuple lst -> fprintf ppf "tuple[%a]" (list_sep type_expression) lst
    | Type_sum m -> fprintf ppf "sum[%a]" (smap_sep type_expression) m
    | Type_record m -> fprintf ppf "record[%a]" (smap_sep type_expression) m
    | Type_function (p, r) -> fprintf ppf "%a -> %a" type_expression p type_expression r
    | Type_variable name -> fprintf ppf "%s" name
    | Type_constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep type_expression) lst

  let literal ppf (l:literal) = match l with
    | Unit -> fprintf ppf "Unit"
    | Bool b -> fprintf ppf "%b" b
    | Number n -> fprintf ppf "%d" n
    | String s -> fprintf ppf "%S" s
    | Bytes b -> fprintf ppf "0x%s" @@ Bytes.to_string @@ Bytes.escaped b

  let rec expression ppf (e:expression) = match e with
    | Literal l -> literal ppf l
    | Variable name -> fprintf ppf "%s" name
    | Application (f, arg) -> fprintf ppf "(%a)@(%a)" annotated_expression f annotated_expression arg
    | Constructor (name, ae) -> fprintf ppf "%s(%a)" name annotated_expression ae
    | Constant (name, lst) -> fprintf ppf "%s(%a)" name (list_sep annotated_expression) lst
    | Tuple lst -> fprintf ppf "tuple[%a]" (list_sep annotated_expression) lst
    | Accessor (ae, p) -> fprintf ppf "%a.%a" annotated_expression ae access_path p
    | Record m -> fprintf ppf "record[%a]" (smap_sep annotated_expression) m
    | Map m -> fprintf ppf "map[%a]" (list_sep assoc_annotated_expression) m
    | LookUp (ds, ind) -> fprintf ppf "(%a)[%a]" annotated_expression ds annotated_expression ind
    | Lambda {binder;input_type;output_type;result;body} ->
        fprintf ppf "lambda (%s:%a) : %a {%a} return %a"
          binder type_expression input_type type_expression output_type
          block body annotated_expression result

  and assoc_annotated_expression ppf : (ae * ae) -> unit = fun (a, b) ->
    fprintf ppf "%a -> %a" annotated_expression a annotated_expression b

  and access ppf (a:access) =
    match a with
    | Tuple_access n -> fprintf ppf "%d" n
    | Record_access s -> fprintf ppf "%s" s

  and access_path ppf (p:access_path) =
    fprintf ppf "%a" (list_sep ~pp_sep:(const ".") access) p

  and type_annotation ppf (ta:type_expression option) = match ta with
    | None -> fprintf ppf ""
    | Some t -> type_expression ppf t

  and annotated_expression ppf (ae:annotated_expression) = match ae.type_annotation with
    | None -> fprintf ppf "%a" expression ae.expression
    | Some t -> fprintf ppf "(%a) : %a" expression ae.expression type_expression t

  and block ppf (b:block) = (list_sep instruction) ppf b

  and single_record_patch ppf ((p, ae) : string * ae) =
    fprintf ppf "%s <- %a" p annotated_expression ae

  and matching ppf (m:matching) = match m with
    | Match_tuple (lst, b) ->
        fprintf ppf "let (%a) = %a" (list_sep (fun ppf -> fprintf ppf "%s")) lst block b
    | Match_bool {match_true ; match_false} ->
        fprintf ppf "| True -> %a @.| False -> %a" block match_true block match_false
    | Match_list {match_nil ; match_cons = (hd, tl, match_cons)} ->
        fprintf ppf "| Nil -> %a @.| %s :: %s -> %a" block match_nil hd tl block match_cons
    | Match_option {match_none ; match_some = (some, match_some)} ->
        fprintf ppf "| None -> %a @.| Some %s -> %a" block match_none some block match_some

  and instruction ppf (i:instruction) = match i with
    | Skip -> fprintf ppf "skip"
    | Fail ae -> fprintf ppf "fail with (%a)" annotated_expression ae
    | Record_patch (name, path, lst) -> fprintf ppf "%s.%a[%a]" name access_path path (list_sep single_record_patch) lst
    | Loop (cond, b) -> fprintf ppf "while (%a) { %a }" annotated_expression cond block b
    | Assignment {name;annotated_expression = ae} ->
        fprintf ppf "%s := %a" name annotated_expression ae
    | Matching (ae, m) ->
        fprintf ppf "match %a with %a" annotated_expression ae matching m

  let declaration ppf (d:declaration) = match d with
    | Type_declaration {type_name ; type_expression = te} ->
        fprintf ppf "type %s = %a" type_name type_expression te
    | Constant_declaration {name ; annotated_expression = ae} ->
        fprintf ppf "const %s = %a" name annotated_expression ae

  let program ppf (p:program) =
    fprintf ppf "%a" (list_sep declaration) p
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
      | Assignment ({name;annotated_expression = e} as a) ->
          let%bind annotated_expression = rename_annotated_expression (filter r name) e in
          ok (Assignment {a with annotated_expression})
      | Skip -> ok Skip
      | Fail e ->
          let%bind e' = rename_annotated_expression r e in
          ok (Fail e')
      | Loop (cond, body) ->
          let%bind cond' = rename_annotated_expression r cond in
          let%bind body' = rename_block r body in
          ok (Loop (cond', body'))
      | Matching (ae, m) ->
          let%bind ae' = rename_annotated_expression r ae in
          let%bind m' = rename_matching r m in
          ok (Matching (ae', m'))
      | Record_patch (v, path, lst) ->
          let aux (x, y) =
            let%bind y' = rename_annotated_expression (filter r v) y in
            ok (x, y') in
          let%bind lst' = bind_map_list aux lst in
          match List.assoc_opt v r with
          | None -> (
              ok (Record_patch (v, path, lst'))
            )
          | Some (v, path') -> (
              ok (Record_patch (v, path' @ path, lst'))
            )
    and rename_block (r:renamings) (bl:block) : block result =
      bind_map_list (rename_instruction r) bl

    and rename_matching (r:renamings) (m:matching) : matching result =
      match m with
      | Match_bool { match_true = mt ; match_false = mf } ->
          let%bind match_true = rename_block r mt in
          let%bind match_false = rename_block r mf in
          ok (Match_bool {match_true ; match_false})
      | Match_option { match_none = mn ; match_some = (some, ms) } ->
          let%bind match_none = rename_block r mn in
          let%bind ms' = rename_block (filter r some) ms in
          ok (Match_option {match_none ; match_some = (some, ms')})
      | Match_list { match_nil = mn ; match_cons = (hd, tl, mc) } ->
          let%bind match_nil = rename_block r mn in
          let%bind mc' = rename_block (filters r [hd;tl]) mc in
          ok (Match_list {match_nil ; match_cons = (hd, tl, mc')})
      | Match_tuple (lst, body) ->
          let%bind body' = rename_block (filters r lst) body in
          ok (Match_tuple (lst, body'))

    and rename_annotated_expression (r:renamings) (ae:annotated_expression) : annotated_expression result =
      let%bind expression = rename_expression r ae.expression in
      ok {ae with expression}

    and rename_expression (r:renamings) (e:expression) : expression result =
      match e with
      | Literal _ as l -> ok l
      | Constant (name, lst) ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (Constant (name, lst'))
      | Constructor (name, ae) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (Constructor (name, ae'))
      | Variable v -> (
          match List.assoc_opt v r with
          | None -> ok (Variable v)
          | Some (name, path) -> ok (Accessor (ae (Variable (name)), path))
        )
      | Lambda ({binder;body;result} as l) ->
          let r' = filter r binder in
          let%bind body = rename_block r' body in
          let%bind result = rename_annotated_expression r' result in
          ok (Lambda {l with body ; result})
      | Application (f, arg) ->
          let%bind f' = rename_annotated_expression r f in
          let%bind arg' = rename_annotated_expression r arg in
          ok (Application (f', arg'))
      | Tuple lst ->
          let%bind lst' = bind_map_list (rename_annotated_expression r) lst in
          ok (Tuple lst')
      | Accessor (ae, p) ->
          let%bind ae' = rename_annotated_expression r ae in
          ok (Accessor (ae', p))
          (* let aux prev hd =
           *   match hd with
           *   | Tuple_access n -> Tuple_accessor (prev, n)
           *   | Record_access s -> Record_accessor (prev, s)
           * in
           * let lst = List.fold_left aux ae p in
           * ok lst *)
      | Record sm ->
          let%bind sm' = bind_smap
            @@ SMap.map (rename_annotated_expression r) sm in
          ok (Record sm')
      | Map m ->
          let%bind m' = bind_map_list
            (fun (x, y) -> bind_map_pair (rename_annotated_expression r) (x, y)) m in
          ok (Map m')
      | LookUp m ->
          let%bind m' = bind_map_pair (rename_annotated_expression r) m in
          ok (LookUp m')
  end
end

module Combinators = struct
  let t_bool      : type_expression = Type_constant ("bool", [])
  let t_string    : type_expression = Type_constant ("string", [])
  let t_bytes     : type_expression = Type_constant ("bytes", [])
  let t_int       : type_expression = Type_constant ("int", [])
  let t_unit      : type_expression = Type_constant ("unit", [])
  let t_option  o : type_expression = Type_constant ("option", [o])
  let t_tuple lst : type_expression = Type_tuple lst
  let t_pair a b = t_tuple [a ; b]
  let t_record m  : type_expression = (Type_record m)
  let t_ez_record (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Type_record map

  let t_record_ez lst =
    let m = SMap.of_list lst in
    t_record m

  let t_sum m : type_expression = Type_sum m
  let make_t_ez_sum (lst:(string * type_expression) list) : type_expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Type_sum map

  let t_function param result : type_expression = Type_function (param, result)

  let annotated_expression ?type_annotation expression = {expression ; type_annotation}

  let name (s : string) : name = s

  let var (s : string) : expression = Variable s

  let unit  () : expression = Literal (Unit)
  let number n : expression = Literal (Number n)
  let bool   b : expression = Literal (Bool b)
  let string s : expression = Literal (String s)
  let bytes  b : expression = Literal (Bytes (Bytes.of_string b))

  let lambda (binder : string)
             (input_type : type_expression)
             (output_type : type_expression)
             (result : expression)
             (body : block)
      : expression =
    Lambda {
        binder = (name binder) ;
        input_type = input_type ;
        output_type = output_type ;
        result = (ae result) ;
        body ;
      }

  let tuple (lst : ae list) : expression = Tuple lst
  let ez_tuple (lst : expression list) : expression =
    tuple (List.map (fun e -> ae e) lst)

  let constructor (s : string) (e : ae) : expression = Constructor (name s, e)

  let record (lst : (string * ae) list) : expression =
    let aux prev (k, v) = SMap.add k v prev in
    let map = List.fold_left aux SMap.empty lst in
    Record map

  let ez_record  (lst : (string * expression) list) : expression =
    (* TODO: define a correct implementation of List.map
     * (an implementation that does not fail with stack overflow) *)
    record (List.map (fun (s,e) -> (s, ae e)) lst)
end
