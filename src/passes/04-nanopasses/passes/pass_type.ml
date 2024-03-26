open Ast_unified
open Simple_utils.Trace
open Simple_utils.Function
include Morphing

type pass = { morphing : morphing }
type raise_t = (Errors.t, Main_warnings.all) raise

module type T = sig
  type flag_arg

  val flag : (bool * flag_arg) option ref
  val set_flag : enable:bool -> flag_arg -> unit
  val is_enabled : unit -> bool
  val name : string
  val reduction : raise:raise_t -> Iter.iter
  val compile : raise:raise_t -> Morphing.pass_kind
  val decompile : raise:raise_t -> Morphing.pass_kind
end

module Selector : sig
  (* select a specific sort transformation from a pass *)
  type 'a t

  val select : 'a t -> morphing -> 'a sub_pass
  val expr : expr t
  val program : program t
  val block : block t
  val pattern : pattern t
  val ty_expr : ty_expr t
  val declaration : declaration t
  val instruction : instruction t
  val sig_expr : sig_expr t
end = struct
  type 'a t = morphing -> 'a sub_pass

  let select (selector : 'a t) (p : morphing) = selector p
  let block x = x.block
  let expr x = x.expression
  let program x = x.program
  let pattern x = x.pattern
  let ty_expr x = x.ty_expr
  let sig_expr x = x.sig_expr
  let declaration x = x.declaration
  let instruction x = x.instruction
end

type pass_direction =
  | Compile
  | Decompile

let process_name =
  (* we use __MODULE__ .. can be a bit incovenient as a name to use with CLI so we process it a bit *)
  let open Simple_utils.Function in
  String.lowercase <@ String.substr_replace_all ~pattern:"Passes__" ~with_:""


type pre_morphing =
  { pass : pass_kind
  ; reduction : Iter.iter
  }

let rec combine_pre_morphings : pre_morphing list -> pre_morphing list = function
  | [] -> []
  | [ last ] -> [ last ]
  | lhs :: rhs :: others ->
    (match Morphing.combine_pass_kinds lhs.pass rhs.pass with
    | None -> lhs :: combine_pre_morphings (rhs :: others)
    | Some combined_pass ->
      let combined_reduction = Iter.combine_iteration [ lhs.reduction; rhs.reduction ] in
      combine_pre_morphings
        ({ pass = combined_pass; reduction = combined_reduction } :: others))


let pass_of_module ({ pass; reduction } : pre_morphing) : pass =
  let morphing = Morphing.morph ~pass ~reduction in
  { morphing }


let passes_of_modules ~raise ~pass_direction (modules : (module T) list) : pass list =
  let pick_pass (module P : T) =
    match pass_direction with
    | Compile -> P.compile ~raise
    | Decompile -> P.decompile ~raise
  in
  let pre_morphings =
    List.filter_map
      ~f:(fun (module P) ->
        Option.some_if
          (P.is_enabled ())
          { pass = pick_pass (module P); reduction = P.reduction ~raise })
      modules
  in
  let combined_pre_morphings = combine_pre_morphings pre_morphings in
  List.map ~f:pass_of_module combined_pre_morphings


(* returns the pass name at which nanopass execution should stop and
   a boolean to know if it is included or not
  e.g. :
  "my_pass" --> "my_pass", false
  "my_pass+" --> "my_pass", true
*)
let partial_pass_execution_from_name stop =
  match String.lsplit2 stop ~on:'+' with
  | Some (name, "") -> true, String.lowercase name
  | _ -> false, String.lowercase stop


let rec select_sub_passes included name : (module T) list -> (module T) list * bool
  = function
  | [] -> [], false
  | (module P) :: tl ->
    if String.equal name (process_name P.name)
    then if included then [ (module P) ], true else [], true
    else
      Tuple2.map_fst ~f:(List.cons (module P : T)) @@ select_sub_passes included name tl


let rec select_passes included name : (module T) list list -> (module T) list list
  = function
  | [] -> []
  | sub_passes :: tl ->
    let selected, found = select_sub_passes included name sub_passes in
    if found then [ selected ] else selected :: select_passes included name tl


(* executes a pass and check combined reductions *)
let compile_with_passes : type a. a -> a sub_pass list -> a =
 fun prg passes ->
  let f : a * a dyn_reduction_check list -> a sub_pass -> a * a dyn_reduction_check list =
   fun (prg, checks) pass ->
    let prg = pass.transformation prg in
    (* checking all the reductions so far *)
    let checks = pass.transformation_check :: checks in
    prg, checks
  in
  let prg, checks = List.fold passes ~init:(prg, []) ~f in
  (combine_checks checks) prg;
  prg


let filter_before (passes : (module T) list list) stop_before : (module T) list list =
  let included, stop = partial_pass_execution_from_name stop_before in
  let all_passes = List.concat passes in
  if not
       (List.exists all_passes ~f:(fun (module P) ->
            String.equal stop (process_name P.name)))
  then failwith "No pass with the specified name";
  if List.exists all_passes ~f:(fun (module P) ->
         String.equal stop (process_name P.name) && not (P.is_enabled ()))
  then failwith "A pass exist with the specified name but isn't enabled";
  select_passes included stop passes


let decompile_passes
    : type a.
      raise:raise_t
      -> ?stop_before:_
      -> sort:a Selector.t
      -> (module T) list list
      -> a
      -> a
  =
 fun ~raise ?stop_before ~sort passes value ->
  passes
  |> List.rev
  |> fun default ->
  Option.value_map stop_before ~default ~f:(filter_before default)
  |> List.concat_map ~f:(passes_of_modules ~pass_direction:Decompile ~raise <@ List.rev)
  |> List.map ~f:(fun (p : pass) -> p.morphing)
  |> List.map ~f:(Selector.select sort)
  |> List.fold ~init:value ~f:(fun prg pass -> pass.transformation prg)


let compile_passes
    : type a.
      raise:raise_t
      -> ?stop_before:_
      -> sort:a Selector.t
      -> (module T) list list
      -> a
      -> a
  =
 fun ~raise ?stop_before ~sort passes prg ->
  passes
  |> fun default ->
  Option.value_map stop_before ~default ~f:(filter_before default)
  |> List.concat_map ~f:(passes_of_modules ~pass_direction:Compile ~raise)
  |> List.map ~f:(fun (p : pass) -> p.morphing)
  |> List.map ~f:(Selector.select sort)
  |> compile_with_passes prg
