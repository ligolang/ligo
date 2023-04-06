open Ast_unified

(* it's preferable to use the defined recursion schemes for code transformations
   but  *)
type 'a code_transformation = 'a -> 'a

(* to dynamically check if reduction happened.
  , the second rhs of the pair allows for combination of reductions checks
*)
type 'a dyn_reduction_check = Iter.iter * (Iter.iter -> 'a -> unit)

type 'a sub_pass =
  { forward : 'a code_transformation
  ; forward_check : 'a dyn_reduction_check
  ; backward : 'a code_transformation
  }

type pass =
  { name : string
  ; expression : expr sub_pass
  ; program : program sub_pass
  ; pattern : pattern sub_pass
  }

let expr_selector x = x.expression
let program_selector x = x.program
let pattern_selector x = x.pattern

let rec select_passes included name passes =
  match passes with
  | [] -> []
  | hd :: tl ->
    if String.equal name hd.name
    then if included then [ hd ] else []
    else hd :: select_passes included name tl


let compile_with_passes : type a. a sub_pass list -> a -> a =
 fun passes prg ->
  let combine_checks : a dyn_reduction_check list -> a -> unit =
   fun checks ->
    let iters = List.map ~f:fst checks in
    let combined_iter = Iter.combine_iteration iters in
    match checks with
    | [] -> fun _ -> ()
    | (_, f) :: _ -> f combined_iter
  in
  let f : a * a dyn_reduction_check list -> a sub_pass -> a * a dyn_reduction_check list =
   fun (prg, checks) pass ->
    let prg = pass.forward prg in
    (* checking all the reductions so far *)
    let checks = pass.forward_check :: checks in
    (combine_checks checks) prg;
    prg, checks
  in
  let prg, _ = List.fold passes ~init:(prg, []) ~f in
  prg


let decompile_with_passes : type a. a sub_pass list -> a -> a =
 fun passes prg -> List.fold passes ~init:prg ~f:(fun prg pass -> pass.backward prg)


let nanopasses_until
    : type a. pass list -> ?stop_before:_ -> selector:(pass -> a sub_pass) -> a -> a
  =
 fun passes ?stop_before ~selector prg ->
  let passes =
    Option.value_map stop_before ~default:passes ~f:(fun n ->
        let included, n =
          match String.lsplit2 n ~on:'+' with
          | Some (name, "") -> true, name
          | _ -> false, n
        in
        let n = String.lowercase n in
        if not (List.exists passes ~f:(fun p -> String.equal n p.name))
        then failwith "No pass with the specified name";
        select_passes included n passes)
  in
  compile_with_passes (List.map ~f:selector passes) prg


type cata_pass =
  ( expr
  , ty_expr
  , pattern
  , statement
  , block
  , mod_expr
  , instruction
  , declaration
  , program_entry
  , program )
  Ast_unified.Catamorphism.fold

let idle_cata_pass : cata_pass =
  { expr = (fun x -> { fp = x })
  ; ty_expr = (fun x -> { fp = x })
  ; pattern = (fun x -> { fp = x })
  ; statement = (fun x -> { fp = x })
  ; block = (fun x -> { fp = x })
  ; mod_expr = (fun x -> { fp = x })
  ; instruction = (fun x -> { fp = x })
  ; declaration = (fun x -> { fp = x })
  ; program_entry = (fun x -> { fp = x })
  ; program = (fun x -> { fp = x })
  }


type 'a pass_unfold =
  ( expr * 'a
  , ty_expr * 'a
  , pattern * 'a
  , statement * 'a
  , block * 'a
  , mod_expr * 'a
  , instruction * 'a
  , declaration * 'a
  , program_entry * 'a
  , program * 'a )
  Ast_unified.Anamorphism.unfold

type 'a pass_fold =
  ( expr * 'a
  , ty_expr * 'a
  , pattern * 'a
  , statement * 'a
  , block * 'a
  , mod_expr * 'a
  , instruction * 'a
  , declaration * 'a
  , program_entry * 'a
  , program * 'a )
  Ast_unified.Catamorphism.fold

let default_unfold : 'a pass_unfold =
  let prop acc x = x, acc in
  { expr =
      (fun (x, acc) ->
        map_expr_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; ty_expr = (fun (x, acc) -> map_ty_expr_ (prop acc) x.fp)
  ; pattern = (fun (x, acc) -> map_pattern_ (prop acc) (prop acc) x.fp)
  ; statement = (fun (x, acc) -> map_statement_ (prop acc) (prop acc) (prop acc) x.fp)
  ; block = (fun (x, acc) -> map_block_ (prop acc) (prop acc) x.fp)
  ; mod_expr = (fun (x, acc) -> map_mod_expr_ (prop acc) (prop acc) x.fp)
  ; instruction =
      (fun (x, acc) ->
        map_instruction_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; declaration =
      (fun (x, acc) ->
        map_declaration_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; program_entry =
      (fun (x, acc) -> map_program_entry_ (prop acc) (prop acc) (prop acc) x.fp)
  ; program = (fun (x, acc) -> map_program_ (prop acc) (prop acc) x.fp)
  }


let default_fold plus init : 'a pass_fold =
  let p acc (_, el) = plus acc el in
  { expr =
      (fun x -> { fp = map_expr_ fst fst fst fst fst x }, fold_expr_ p p p p p init x)
  ; ty_expr = (fun x -> { fp = map_ty_expr_ fst x }, fold_ty_expr_ p init x)
  ; pattern = (fun x -> { fp = map_pattern_ fst fst x }, fold_pattern_ p p init x)
  ; statement =
      (fun x -> { fp = map_statement_ fst fst fst x }, fold_statement_ p p p init x)
  ; block = (fun x -> { fp = map_block_ fst fst x }, fold_block_ p p init x)
  ; mod_expr = (fun x -> { fp = map_mod_expr_ fst fst x }, fold_mod_expr_ p p init x)
  ; instruction =
      (fun x ->
        ( { fp = map_instruction_ fst fst fst fst fst x }
        , fold_instruction_ p p p p p init x ))
  ; declaration =
      (fun x ->
        ( { fp = map_declaration_ fst fst fst fst fst x }
        , fold_declaration_ p p p p p init x ))
  ; program_entry =
      (fun x ->
        { fp = map_program_entry_ fst fst fst x }, fold_program_entry_ p p p init x)
  ; program = (fun x -> { fp = map_program_ fst fst x }, fold_program_ p p init x)
  }


type 'a pass_kind =
  [ `Cata of cata_pass
  | `Hylo of 'a pass_fold * 'a pass_unfold
  | `Check of Iter.iter
  | `None
  ]

let morph
    ~name
    ~(compile : 'a pass_kind)
    ~(decompile : 'a pass_kind)
    ~(reduction_check : Ast_unified.Iter.iter)
    : pass
  =
  let mk_morph pass_kind (catapass, (cata, ana), check) value =
    match pass_kind with
    | `Cata pass -> catapass ~f:pass value
    | `Hylo (fold, unfold) -> ana ~f:unfold (cata ~f:fold value)
    | `Check pass ->
      check ~f:pass value;
      value
    | `None -> value
  in
  let mk_sub_pass
      : type v.
        (f:cata_pass -> v -> v)
        * ((f:'a pass_fold -> v -> v * 'a) * (f:'a pass_unfold -> v * 'a -> v))
        * (f:Iter.iter -> v -> unit)
        -> v sub_pass
    =
   fun (cata, ana, check) ->
    { forward = mk_morph compile (cata, ana, check)
    ; forward_check = (reduction_check, fun f v -> check ~f v)
    ; backward = mk_morph decompile (cata, ana, check)
    }
  in
  let expression =
    mk_sub_pass
      ( Catamorphism.cata_expr
      , (Catamorphism.cata_expr, Anamorphism.ana_expr)
      , Iter.iter_expr )
  in
  let program =
    mk_sub_pass
      ( Catamorphism.cata_program
      , (Catamorphism.cata_program, Anamorphism.ana_program)
      , Iter.iter_program )
  in
  let pattern =
    mk_sub_pass
      ( Catamorphism.cata_pattern
      , (Catamorphism.cata_pattern, Anamorphism.ana_pattern)
      , Iter.iter_pattern )
  in
  let process_name =
    (* we use __MODULE__ .. can be a bit incovenient as a name to use with CLI so we process it a bit *)
    let open Simple_utils.Function in
    String.lowercase <@ String.substr_replace_all ~pattern:"Passes__" ~with_:""
  in
  { name = process_name name; expression; program; pattern }
