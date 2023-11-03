open Ast_unified
open Simple_utils.Function

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

type morphing =
  { expression : expr sub_pass
  ; program : program sub_pass
  ; pattern : pattern sub_pass
  ; ty_expr : ty_expr sub_pass
  ; instruction : instruction sub_pass
  ; block : block sub_pass
  ; declaration : declaration sub_pass
  ; sig_expr : sig_expr sub_pass
  }

let idle_fold = Catamorphism.idle

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
  , program * 'a
  , sig_expr * 'a
  , sig_entry * 'a )
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
  , program * 'a
  , sig_expr * 'a
  , sig_entry * 'a )
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
        map_declaration_
          (prop acc)
          (prop acc)
          (prop acc)
          (prop acc)
          (prop acc)
          (prop acc)
          x.fp)
  ; program_entry =
      (fun (x, acc) -> map_program_entry_ (prop acc) (prop acc) (prop acc) x.fp)
  ; program = (fun (x, acc) -> map_program_ (prop acc) (prop acc) x.fp)
  ; sig_expr = (fun (x, acc) -> map_sig_expr_ (prop acc) (prop acc) (prop acc) x.fp)
  ; sig_entry = (fun (x, acc) -> map_sig_entry_ (prop acc) (prop acc) (prop acc) x.fp)
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
        ( { fp = map_declaration_ fst fst fst fst fst fst x }
        , fold_declaration_ p p p p p p init x ))
  ; program_entry =
      (fun x ->
        { fp = map_program_entry_ fst fst fst x }, fold_program_entry_ p p p init x)
  ; program = (fun x -> { fp = map_program_ fst fst x }, fold_program_ p p init x)
  ; sig_expr =
      (fun x -> { fp = map_sig_expr_ fst fst fst x }, fold_sig_expr_ p p p init x)
  ; sig_entry =
      (fun x -> { fp = map_sig_entry_ fst fst fst x }, fold_sig_entry_ p p p init x)
  }


let default_refold_acc : plus:('a -> 'a -> 'a) -> init:'a -> 'a pass_fold * 'a pass_unfold
  =
 fun ~plus ~init -> default_fold plus init, default_unfold


let combine_checks : type a. a dyn_reduction_check list -> a -> unit =
 fun checks ->
  let iters = List.map ~f:fst checks in
  let combined_iter = Iter.combine_iteration iters in
  match checks with
  | [] -> fun _ -> ()
  | (_, f) :: _ -> f combined_iter


type pass_kind =
  | Seq : pass_kind * pass_kind -> pass_kind
  | Ignore : pass_kind -> pass_kind
  | Fold : Catamorphism.idle_fold -> pass_kind
  | Refold_acc : 'a pass_fold * 'a pass_unfold -> pass_kind
  | Check : Iter.iter -> pass_kind
  | Nothing : pass_kind

type 'ast morphers =
  { cata : f:Catamorphism.idle_fold -> 'ast -> 'ast
  ; hylo :
      'acc.
      (f:'acc pass_fold -> 'ast -> 'ast * 'acc)
      * (f:'acc pass_unfold -> 'ast * 'acc -> 'ast)
  ; iter : f:Iter.iter -> 'ast -> unit
  }

let rec mk_morph : pass_kind -> 'b morphers -> 'b -> 'b =
 fun pass_kind ({ cata; hylo = h_cata, h_ana; iter } as morph) b ->
  match pass_kind with
  | Fold f -> cata ~f b
  | Seq (p, q) -> mk_morph q morph (mk_morph p morph b)
  | Ignore p ->
    let _ = mk_morph p morph b in
    b
  | Refold_acc (fold, unfold) -> (h_ana ~f:unfold <@ h_cata ~f:fold) b
  | Check pass ->
    iter ~f:pass b;
    b
  | Nothing -> b


let _type__morphers =
  { cata = Catamorphism.cata__type_
  ; hylo = Catamorphism.cata__type_, Anamorphism.ana__type_
  ; iter = Iter.iter__type_
  }
  [@@map
    _type_
    , ( "expr"
      , "program"
      , "pattern"
      , "ty_expr"
      , "instruction"
      , "block"
      , "declaration"
      , "sig_expr" )]


let mk_code_transformation : type v. pass_kind -> v morphers -> v code_transformation =
 fun pass_kind morphers -> mk_morph pass_kind morphers


let morph
    ~(compile : pass_kind)
    ~(decompile : pass_kind)
    ~(reduction : Ast_unified.Iter.iter)
    : morphing
  =
  let mk_sub_pass ({ iter; _ } as morphers) =
    { forward = mk_code_transformation compile morphers
    ; forward_check = (reduction, fun f v -> iter ~f v)
    ; backward = mk_code_transformation decompile morphers
    }
  in
  { expression = mk_sub_pass expr_morphers
  ; program = mk_sub_pass program_morphers
  ; pattern = mk_sub_pass pattern_morphers
  ; ty_expr = mk_sub_pass ty_expr_morphers
  ; block = mk_sub_pass block_morphers
  ; declaration = mk_sub_pass declaration_morphers
  ; instruction = mk_sub_pass instruction_morphers
  ; sig_expr = mk_sub_pass sig_expr_morphers
  }
