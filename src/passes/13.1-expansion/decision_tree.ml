(** This module implements the algorithm for pattern matching compiler from
    the paper "Compiling Pattern Matching to Good Decision Trees" by Luc Maranget

    The patten matching compiler tranforms patterns in an match expression to
    nested match expressions containing simple patterns
    e.g. a match expression like
      match x, y with
      | A, B -> (* 1 *)
      | B, A -> (* 2 *)
      | _    -> (* 3 *)
    gets transformed to
      match x with
      | A ->
          match y with
          | A -> (* 3 *) 
          | B -> (* 1 *)
      | B ->
          match y with
          | A -> (* 2 *)
          | B -> (* 3 *)

    Some terminology used in the paper
    1. Signature: Set of all constructors of a sum type
    2. Matrix (P -> A): Match expressions are represented as matrices, each row
       corresponds to a clause in the match. A row looks like (pi1, ..., pin) -> ai
       Here p's are patterns and a's are the body of clause
    3. Occrance: Occurance is a path to a sub-pattern. 
       e.g. | A(D(E)), B, C -> ...
       The occurance of E is x.A.D.E 
    4. Head constructor: Head constructor of a pattern is the set of constructors
       that occur in the first column of the matrix.
       e.g. Head constructor of (A, B, _, C) is {A}
            Head constrcutor of (_, B, _, C) is {}
    
    Future optimizations
    1. Implement necessity based heuristics: This module implements the basic algorithm
       described in the paper. In the paper the author also describes some heuristics
       for column selection which leads to more compact decision trees.
    2. Implement maximal sharing: The bodies of clauses are sometimes repeated many times
       when wildcard pattern are used. If these bodies of caluses are large this may
       increses the code-size if they are repeated. To avoid this probelm we can lift
       the bodies of clauses into functions and replace the bodies by call to the 
       corresponding functions. 
 *)
module Location = Simple_utils.Location

module Value_var = Ligo_prim.Value_var
module Binder = Ligo_prim.Binder
module Label = Ligo_prim.Label
module Record = Ligo_prim.Record
module I = Ast_aggregated
module O = Ast_expanded
module C = Ast_aggregated.Combinators
module ConstructorSet = Set.Make (Label)
module ConstructorMap = Label.Map
module OccuranceMap = Label.Map

let label_map_of_kv_list : (Label.t * 'value) list -> 'value OccuranceMap.t =
 fun lvlist ->
  List.fold lvlist ~init:OccuranceMap.empty ~f:(fun lm (k, v) ->
      OccuranceMap.update lm k ~f:(function
          | Some v -> v
          | None -> v))


module VSet = Set.Make (Value_var)

(** The [Names] module keeps track of all the occurances and their corresponding
    generated variables and also all the bound variables in patterns *)
module Names = struct
  type t = Value_var.t OccuranceMap.t

  let init matchee_label matchee_var = OccuranceMap.singleton matchee_label matchee_var

  let get : Label.t -> t -> Value_var.t * t =
   fun label names ->
    match OccuranceMap.find names label with
    | Some v -> v, names
    | None ->
      let v = Value_var.fresh ~loc:Location.generated ~name:(Label.to_string label) () in
      ( v
      , OccuranceMap.update names label ~f:(function
            | None -> v
            | Some v -> v) )
end

type signature = ConstructorSet.t

type simple_pattern =
  | SP_Var of Value_var.t * (O.type_expression[@sexp.opaque])
  | SP_Constructor of
      { name : Label.t
      ; args : simple_pattern list
      ; args_type : (O.type_expression[@sexp.opaque])
      ; signature : (signature[@sexp.opaque])
      ; parent_type : (O.type_expression[@sexp.opaque])
      ; location : (Location.t[@sexp.opaque])
      }
[@@deriving sexp]

type action = (O.expression[@sexp.opaque]) [@@deriving sexp]
type row = simple_pattern list * action [@@deriving sexp]
type matrix = row list [@@deriving sexp]
type occurance = Label.t * O.type_expression
type occurances = occurance list

let get_number_of_fields : O.type_expression -> int =
 fun t ->
  match C.get_t_record t with
  | Some row -> I.Row.length row
  | None -> 1


let rec n_vars ~loc n =
  if n = 0
  then []
  else
    SP_Var (Value_var.fresh ~loc (), I.Combinators.t_unit ~loc ()) :: n_vars ~loc (n - 1)


let empty_label = Label.of_string ""
let nil_label = Label.of_string "Nil"
let cons_label = Label.of_string "Cons"
let list_signature = ConstructorSet.of_list [ nil_label; cons_label ]

let nil ty parent_type loc =
  SP_Constructor
    { name = nil_label
    ; args = []
    ; args_type = ty
    ; signature = list_signature
    ; parent_type
    ; location = loc
    }


let cons hd tl ty parent_type loc =
  SP_Constructor
    { name = cons_label
    ; args = hd @ tl
    ; args_type = ty
    ; signature = list_signature
    ; parent_type
    ; location = loc
    }


(* Helpers of converting [P_list] to Cons & Nil *)
let p_unit ~loc = Location.wrap ~loc I.Pattern.P_unit
let p_variant ~loc c p = Location.wrap ~loc (I.Pattern.P_variant (c, p))
let p_pair ~loc a b = Location.wrap ~loc (I.Pattern.P_tuple [ a; b ])
let p_cons ~loc a b = p_variant ~loc cons_label (p_pair ~loc a b)
let p_nil ~loc = p_variant ~loc nil_label (p_unit ~loc)

(* Helper for joining [Label.t] *)
let join_labels ls = List.fold_right ls ~init:empty_label ~f:Label.join

let corner_case : type a. string -> string -> unit -> a =
 fun msg loc () -> failwith (Format.asprintf "Corner case: %s at %s" msg loc)


let corner_case_constructor_not_found c ty =
  corner_case
    (Format.asprintf
       "constructor %a not found in type %a"
       Label.pp
       c
       O.PP.type_expression
       ty)


let rec fold_pattern_with_path
    : (Label.t -> 'a -> 'b I.Pattern.t -> 'a) -> 'a -> Label.t -> 'b I.Pattern.t -> 'a
  =
 fun f acc path p ->
  let loc = Location.get_location p in
  let acc = f path acc p in
  let open I.Pattern in
  match Location.unwrap p with
  | P_unit -> acc
  | P_var _ -> acc
  | P_list (Cons (h, t)) -> fold_pattern_with_path f acc path (p_cons ~loc h t)
  | P_list (List ps) ->
    let rec aux ps =
      match ps with
      | [] -> p_nil ~loc
      | p :: ps ->
        let ps = aux ps in
        p_cons ~loc p ps
    in
    fold_pattern_with_path f acc path (aux ps)
  | P_variant (c, p) ->
    let path = Label.join path c in
    fold_pattern_with_path f acc path p
  | P_tuple lp ->
    List.foldi
      ~f:(fun i acc p ->
        let path = Label.join path (Label.of_int i) in
        fold_pattern_with_path f acc path p)
      ~init:acc
      lp
  | P_record lps ->
    Record.foldi
      ~f:(fun l acc p ->
        let path = Label.join path l in
        fold_pattern_with_path f acc path p)
      ~init:acc
      lps


type vars_and_projections =
  { bound_var : Value_var.t
  ; new_var : Value_var.t
  ; ascr : O.type_expression
  }

(** This function scans a pattern an find occurances for each sub-pattern
    and generates fresh variables for them and also return a list of 
    [vars_and_projections] i.e. a list of variables to be substituted by
    new fresh variables *)
let get_vars_and_projections
    :  Label.t -> Names.t -> O.type_expression I.Pattern.t
    -> vars_and_projections list * Names.t
  =
 fun path names p ->
  fold_pattern_with_path
    (fun path (vp, names) p ->
      let _, names = Names.get path names in
      match Location.unwrap p with
      | I.Pattern.P_var b ->
        let bound_var = Binder.get_var b in
        let new_var, names = Names.get path names in
        let ascr = Binder.get_ascr b in
        { bound_var; new_var; ascr } :: vp, names
      | _ -> vp, names)
    ([], names)
    path
    p


(** This functions get the type of argument of a constructor.
    e.g. type a = A of int * int * int | B of { a : int ; b : int } | C of int
    get_variant_nested_type A a => int * int * int 
    get_variant_nested_type B a => { a : int ; b : int } 
    get_variant_nested_type C a => int *)
let get_variant_nested_type c ty =
  match C.get_t_sum ty with
  | Some tsum ->
    let label_map = tsum.fields in
    Option.value_or_thunk ~default:(corner_case_constructor_not_found c ty __LOC__)
    @@ Label.Map.find label_map c
  | None ->
    if C.is_t_list ty
    then
      if Label.equal c cons_label
      then
        C.t_record
          ~loc:Location.generated
          (I.Row.create_tuple [ Option.value_exn ~here:[%here] @@ C.get_t_list ty; ty ])
          ()
      else C.t_unit ~loc:Location.generated ()
    else corner_case "can't get nested type of non variant" __LOC__ ()


(** This function gets all the constructors of a sum type *)
let get_signature_of_sum_type : O.type_expression -> signature =
 fun t ->
  let row = Option.value_exn ~here:[%here] (C.get_t_sum t) in
  let cts = I.Row.to_alist row in
  let cs = List.map cts ~f:fst in
  ConstructorSet.of_list cs


(** This function converts [I.Pattern.t] to [simple_pattern list]
    record [P_record] & tuple [P_tuple] patterns are simplified by representing
    then as list of [simple_pattern]'s  *)
let rec to_simple_pattern
    : O.type_expression -> O.type_expression I.Pattern.t -> simple_pattern list
  =
 fun ty p ->
  let open I.Pattern in
  let loc = Location.get_location p in
  let is_unit_pattern : _ I.Pattern.t -> bool =
   fun p ->
    match Location.unwrap p with
    | P_unit -> true
    | _ -> false
  in
  match Location.unwrap p with
  | P_unit ->
    assert (C.is_t_unit ty);
    let v = Value_var.fresh ~loc ~name:"unit_pattern" () in
    [ SP_Var (v, ty) ]
  | P_var b ->
    let loc = Binder.get_loc b in
    n_vars ~loc (get_number_of_fields ty)
  | P_list (List []) -> [ nil (get_variant_nested_type nil_label ty) ty loc ]
  | P_list (List ps) ->
    let hd_ty = Option.value_exn ~here:[%here] (C.get_t_list ty) in
    List.fold_right
      ps
      ~init:[ nil (get_variant_nested_type nil_label ty) ty loc ]
      ~f:(fun hd tl ->
        let hd = to_simple_pattern hd_ty hd in
        [ cons hd tl (get_variant_nested_type cons_label ty) ty loc ])
  | P_list (Cons (h, t)) ->
    let hd_ty = Option.value_exn ~here:[%here] (C.get_t_list ty) in
    let h = to_simple_pattern hd_ty h in
    let t = to_simple_pattern ty t in
    [ cons h t (get_variant_nested_type cons_label ty) ty loc ]
  | P_variant (c, p) ->
    let args_type = get_variant_nested_type c ty in
    let signature = get_signature_of_sum_type ty in
    let ps = if is_unit_pattern p then [] else to_simple_pattern args_type p in
    [ SP_Constructor
        { name = c; args = ps; args_type; signature; parent_type = ty; location = loc }
    ]
  | P_tuple ps ->
    let row = Option.value_exn ~here:[%here] (C.get_t_record ty) in
    List.concat_mapi ps ~f:(fun i p ->
        let p_ty =
          Option.value_exn ~here:[%here] (OccuranceMap.find row.fields (Label.of_int i))
        in
        to_simple_pattern p_ty p)
  | P_record ps ->
    let row = Option.value_exn ~here:[%here] (C.get_t_record ty) in
    let ps = Record.to_list ps in
    List.concat_map ps ~f:(fun (label, p) ->
        let p_ty = Option.value_exn ~here:[%here] (OccuranceMap.find row.fields label) in
        to_simple_pattern p_ty p)


(** This function scans a pattern and returns a list of [occurance] *)
let get_occurances : O.type_expression -> occurances =
 fun t ->
  if C.is_t_unit t
  then []
  else if C.is_t_record t
  then (
    let rec aux : O.type_expression -> occurances =
     fun t ->
      match C.get_t_record t with
      | Some row ->
        let fields = I.Row.to_alist row in
        List.concat_map fields ~f:(fun (l, ty) ->
            let t = aux ty in
            if List.is_empty t
            then [ l, ty ]
            else List.map ~f:(fun (l', t) -> join_labels [ l; l' ], t) t)
      | None -> []
    in
    aux t)
  else [ empty_label, t ]


(** This function takes a list of [occurance] (o1, o2, ... , on) and returns the 
    specialized occurances (o1.1, ..., o1.a, o2, ..., on) 
    where a is the arity of the constructor *)
let specialize_occurances : Label.t -> occurances -> O.type_expression -> occurances =
 fun c os ty ->
  match os with
  | [] -> corner_case "can't specialize occurances" __LOC__ ()
  | (o1, _) :: o2_n ->
    let o1_a =
      List.map (get_occurances ty) ~f:(fun (oi, ti) -> join_labels [ o1; c; oi ], ti)
    in
    o1_a @ o2_n


(** This function calculates the specialized matrix of the given matrix
    +-----------------------------+-------------------------------------------+
    |  Pattern pi1                |  Row of default matrix                    |
    +-----------------------------+-------------------------------------------+
    | c (q1, ..., qa)             | q1, ..., qa, pi2, ..., pin -> ai          |
    | c' (q1, ..., qa)  (c != c') | No row                                    |
    | _                           | _, ... a times ... _, pi2, ..., pin -> ai |
    +-----------------------------+-------------------------------------------+  
*)
let specialize : Label.t -> int -> matrix -> matrix =
 fun c a matrix ->
  List.filter_map matrix ~f:(fun (pattern, body) ->
      match pattern with
      | [] -> Some ([], body)
      | SP_Constructor
          { name = c'; args = qs; args_type = _; signature = _; parent_type = _ }
        :: ps
        when Label.equal c c' -> Some (qs @ ps, body)
      | SP_Constructor _ :: _ -> None
      | SP_Var (v, _) :: ps ->
        let loc = Value_var.get_location v in
        Some (n_vars ~loc a @ ps, body))


(** This function takes a list of [occurance] (o1, o2, ... , on) and returns the 
    default occurances (o2, ..., on) *)
let default_occurances : occurances -> occurances =
 fun os ->
  match os with
  | [] -> corner_case "can't get default occurances" __LOC__ ()
  | _ :: o2_n -> o2_n


(** This function calculates the default matrix of the given matrix
    +-------------------------+-------------------------+
    |  Pattern pi1            |  Row of default matrix  |
    +-------------------------+-------------------------+
    | c (q1, ..., qa)         | No row                  |
    |                         |                         |
    | _                       | pi2 ... p2n -> ai       |
    +-------------------------+-------------------------+           
    This tranformation filters out all the row with constructors in the first 
    column *)
let default : matrix -> matrix =
 fun matrix ->
  List.filter_map matrix ~f:(fun (pattern, body) ->
      match pattern with
      | [] -> Some ([], body)
      | SP_Constructor _ :: _ -> None
      | SP_Var _ :: ps -> Some (ps, body))


type decision_tree =
  | Leaf of action
  | Fail
  | SwitchConstructor of
      { matchee : (O.type_expression[@sexp.opaque]) Binder.t
      ; cases : subtree list
      ; variant_type : (O.type_expression[@sexp.opaque])
      ; location : (Location.t[@sexp.opaque])
      }
  | SwitchRecord of
      { matchee : (O.type_expression[@sexp.opaque]) Binder.t
      ; field_binders : (O.type_expression[@sexp.opaque]) Binder.t Record.t
      ; case : decision_tree
      ; record_type : (O.type_expression[@sexp.opaque])
      ; location : (Location.t[@sexp.opaque])
      }

and subtree =
  { constructor : Label.t
  ; binder : Value_var.t
  ; subtree : decision_tree
  }
[@@deriving sexp]

(** This is a predicate function which returns [true] is all the patterns are
    [SP_Var] *)
let has_all_var_pattersn : simple_pattern list -> bool =
 fun pattern ->
  List.for_all pattern ~f:(fun r ->
      match r with
      | SP_Var _ -> true
      | SP_Constructor _ -> false)


type constructor_info =
  { arity : int
  ; args_type : O.type_expression
  ; parent_type : O.type_expression
  ; location : Location.t
  }

type head_constructors_info =
  { index : int
  ; head_constructors : constructor_info ConstructorMap.t
  ; signature : signature
  }

(** This functions returns info about constructor in the 1 st column of the [row]
    It returns [None] if the 1 st [row] is not a constructor *)
let get_first_column_constructor
    : row -> ((Label.t * constructor_info) * signature) option
  =
 fun (pattern, _) ->
  match pattern with
  | [] -> None
  | SP_Var _ :: _ -> None
  | SP_Constructor { name = c; args = ps; args_type; signature; parent_type; location }
    :: _ ->
    let arity = List.length ps in
    Some ((c, { arity; args_type; parent_type; location }), signature)


let remove_first_column : row -> row =
 fun (pattern, body) ->
  match pattern with
  | [] -> [], body
  | _ :: rows -> rows, body


(** This function returns the index and other info of the column which has at least
    on constructor pattern *)
let head_constructors_of_column_with_atleast_one_constructor
    : matrix -> head_constructors_info
  =
 fun matrix ->
  let rec aux : matrix -> int -> head_constructors_info =
   fun rows idx ->
    let first_column_constructors, signatures =
      List.unzip @@ List.filter_map rows ~f:get_first_column_constructor
    in
    if List.is_empty first_column_constructors
    then (
      let matrix = List.map rows ~f:remove_first_column in
      aux matrix (idx + 1))
    else (
      let signature =
        Option.value_or_thunk ~default:(corner_case "signature list is empty" __LOC__)
        @@ List.hd signatures
      in
      let head_constructors = label_map_of_kv_list first_column_constructors in
      { index = idx; head_constructors; signature })
  in
  aux matrix 0


(** This function swaps i th element with first element.
    e.g. swap_elt 3 [ 1 ; 2 ; 3 ; 4 ; 5 ]
      => [ 4 ; 2 ; 3 ; 1 ; 5 ]  *)
let swap_elt : int -> 'a list -> 'a list =
 fun idx xs ->
  let rec aux : int -> 'a list -> 'a list * 'a * 'a list =
   fun idx xs ->
    match xs with
    | [] -> corner_case "cannot swap row" __LOC__ ()
    | x :: xs when idx = 0 -> [], x, xs
    | x :: xs ->
      let start, elt, rest = aux (idx - 1) xs in
      x :: start, elt, rest
  in
  let start, elt, rest = aux idx xs in
  match start with
  | [] -> xs
  | h :: t -> (elt :: t) @ (h :: rest)


(** This function swaps i th column in the matrix with the first column *)
let swap_column_in_matrix : int -> matrix -> matrix =
 fun i matrix -> List.map matrix ~f:(fun (pattern, body) -> swap_elt i pattern, body)


(** This function checks if a the [decision_tree] need to be wrapped in a [SwitchRecord]
    and returns a boolean indicating if wrapping is needed and new decision tree
    which wraps the old decision tree with a [SwitchRecord] *)
let rec generate_match_record
    :  Location.t -> Label.t -> O.type_expression -> Names.t -> decision_tree
    -> decision_tree
  =
 fun location matchee_label record_type names dt ->
  match C.get_t_record record_type with
  | Some row ->
    let fields = I.Row.to_alist row in
    let names, field_binders =
      List.fold_map fields ~init:names ~f:(fun names (field_label, t) ->
          let label = join_labels [ matchee_label; field_label ] in
          let var, names = Names.get label names in
          let binder = Binder.make var t in
          names, (field_label, binder))
    in
    let case =
      List.fold fields ~init:dt ~f:(fun dt (l, ty) ->
          let v = join_labels [ matchee_label; l ] in
          generate_match_record location v ty names dt)
    in
    let field_binders = Record.of_list field_binders in
    let matchee_var, _ = Names.get matchee_label names in
    let matchee = Binder.make matchee_var record_type in
    SwitchRecord { matchee; field_binders; case; record_type; location }
  | None -> dt


(** This is the core function which transforms the [matrix] into [decision_tree]
    The algorithm is as follows
    1. If matrix has no rows then we emit the [Fail] node (this case happens when
       a match is not well-formed)
    2. If first row of matrix has no columns or only var patterns matching always
       succeeds and we emit a [Leaf] node with the action/body of the first row
    3. In any other case the matrix has more than one row and column.
       Here we look for a column index i such that there is at least one constructor
       pattern. 
       a. Let us consider that i = 0 (first column)
          We get the set of head constructors of the first column call it Σ1
          
            For each constructor ck we find the [subtree] like
            Ak = to_decision_tree((o1·1 ... o1·a o2 ... on), S(ck, P → A))
            i.e. we specialize the occurances & the marix

            Then we group togher the [subree]s in a list like
            L = c1:A1;c2:A2;...;ck:Ak

          If Σ1 is a signature (i.e. it contains all constructors of the sum type)
          then
            and we emit the [SwitchConstructor] node the the list of subrers L

          If Σ1 is not a signature then we perform additional recursive calls
          for the missing constructors
            Am = to_decision_tree((o2 ... on), S(ck, P → A))

            Then we group together all the subtrees
            L = c1:A1;c2:A2;...;ck:Ak;cm1:Am1;...cmn:Amn

            and we emit the [SwitchConstructor] node the the list of subrers L *)
let rec to_decision_tree : Names.t -> occurances -> matrix -> decision_tree =
 fun names occurances matrix ->
  match matrix with
  | [] -> Fail
  | (row, body) :: _ when List.is_empty row -> Leaf body
  | (row, body) :: _ when has_all_var_pattersn row -> Leaf body
  | matrix ->
    let { index = i; head_constructors = head_constructors_with_arity; signature } =
      head_constructors_of_column_with_atleast_one_constructor matrix
    in
    if i = 0
    then (
      let o, ot =
        Option.value_or_thunk ~default:(corner_case "empty occurances" __LOC__)
        @@ List.hd occurances
      in
      let head_constructors =
        ConstructorSet.of_list @@ ConstructorMap.keys head_constructors_with_arity
      in
      let missing_constructors = ConstructorSet.diff signature head_constructors in
      let head, parent_tys, locations =
        List.unzip3
        @@ ConstructorMap.fold
             head_constructors_with_arity
             ~init:[]
             ~f:(fun ~key:c ~data l ->
               let { arity = a; args_type = ty; parent_type = parent_ty; location = loc } =
                 data
               in
               let t =
                 to_decision_tree
                   names
                   (specialize_occurances c occurances ty)
                   (specialize c a matrix)
               in
               let constructor_label = join_labels [ o; c ] in
               (* If a constructor pattern contains are record pattern we wrap the 
                  decision tree in a [SwitchRecord] *)
               let t = generate_match_record loc constructor_label ty names t in
               let constructor_var, _ = Names.get constructor_label names in
               ({ constructor = c; binder = constructor_var; subtree = t }, parent_ty, loc)
               :: l)
      in
      let default =
        List.map (ConstructorSet.to_list missing_constructors) ~f:(fun c ->
            let label = join_labels [ o; c ] in
            let binder, names = Names.get label names in
            let subtree =
              to_decision_tree names (default_occurances occurances) (default matrix)
            in
            { constructor = c; binder; subtree })
      in
      let variant_type =
        Option.value_or_thunk ~default:(corner_case "empty parent_tys" __LOC__)
        @@ List.hd parent_tys
      in
      let location =
        Option.value_or_thunk ~default:(corner_case "empty locations" __LOC__)
        @@ List.hd locations
      in
      let o, _ = Names.get o names in
      let cases =
        if ConstructorSet.is_empty missing_constructors then head else head @ default
      in
      SwitchConstructor { matchee = Binder.make o ot; cases; variant_type; location })
    else (
      let swapped_matrix = swap_column_in_matrix i matrix in
      let swapped_occurances = swap_elt i occurances in
      to_decision_tree names swapped_occurances swapped_matrix)


(** This function is quite straightforward conversion from [decision_tree] to
   [O.E_matching] *)
let rec to_match_expression : body_type:O.type_expression -> decision_tree -> O.expression
  =
 fun ~body_type tree ->
  match tree with
  | Fail ->
    (* This case will likely never occur because we do the anomaly detection in 
      an earlier pass, this case will happen only when match expression is not 
      well-formed (redundant cases or non-exhaustive match) *)
    corner_case
      "not a well-formed match expression, this should have been caught while typing"
      __LOC__
      ()
  | Leaf e ->
    (* Body of the case *)
    e
  | SwitchRecord { matchee; field_binders; case; record_type; location = loc } ->
    (* This is straightforward conversion to Match_record *)
    let matchee =
      O.make_e
        ~loc:(Binder.get_loc matchee)
        (O.E_variable (Binder.get_var matchee))
        (Binder.get_ascr matchee)
    in
    let body = to_match_expression ~body_type case in
    let cases = O.Match_record { fields = field_binders; body; tv = record_type } in
    O.e_matching ~loc { matchee; cases } body_type
  | SwitchConstructor { matchee; cases; variant_type; location = loc } ->
    (* This is straightforward conversion to Match_variant *)
    let matchee =
      O.make_e
        ~loc:(Binder.get_loc matchee)
        (O.E_variable (Binder.get_var matchee))
        (Binder.get_ascr matchee)
    in
    let cases =
      List.map cases ~f:(fun case ->
          let body = to_match_expression ~body_type case.subtree in
          let pattern = case.binder in
          let constructor = case.constructor in
          { O.constructor; pattern; body })
    in
    let cases = O.Match_variant { cases; tv = variant_type } in
    O.e_matching ~loc { matchee; cases } body_type


let compile
    (matchee_type : O.type_expression)
    (matchee : Value_var.t)
    (cases : (O.expression, O.type_expression) I.Match_expr.match_case list)
    ~(mut : bool)
    : O.expression
  =
  let matchee_label = Label.of_string (Format.asprintf "%a" Value_var.pp matchee) in
  let matchee_loc = Value_var.get_location matchee in
  let body_type =
    let first_case =
      Option.value_or_thunk
        ~default:(corner_case "empty cases in match expression" __LOC__)
      @@ List.hd cases
    in
    first_case.body.type_expression
  in
  let names = Names.init matchee_label matchee in
  let names, matrix =
    List.fold_map cases ~init:names ~f:(fun names { pattern; body } ->
        let vars_projs, names = get_vars_and_projections matchee_label names pattern in
        let body =
          List.fold
            vars_projs
            ~init:body
            ~f:(fun body { bound_var = to_subst; new_var; ascr } ->
              (if mut then O.e_let_mut_in else O.e_let_in)
                { let_binder = Binder.make to_subst ascr
                ; rhs = O.e_variable new_var ascr ~loc:Location.generated
                ; let_result = body
                ; (* ??? *)
                  attributes =
                    { inline = false
                    ; no_mutation = false
                    ; view = false
                    ; entry = false
                    ; public = false
                    ; hidden = false
                    ; thunk = false
                    }
                }
                body_type
                ~loc:Location.generated)
        in
        names, (to_simple_pattern matchee_type pattern, body))
  in
  let top_occurances =
    List.map ~f:(fun (l, t) -> join_labels [ matchee_label; l ], t)
    @@ get_occurances matchee_type
  in
  let dt = to_decision_tree names top_occurances matrix in
  (* If the top-level pattern is a record pattern we wrap the decision tree in a
     [SwitchRecord] *)
  let dt = generate_match_record matchee_loc matchee_label matchee_type names dt in
  let match_expr = to_match_expression ~body_type dt in
  match_expr
