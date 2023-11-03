open Ast_unified
open Simple_utils.Trace
include Morphing

type pass =
  { name : string
  ; morphing : morphing
  }

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

let process_name =
  (* we use __MODULE__ .. can be a bit incovenient as a name to use with CLI so we process it a bit *)
  let open Simple_utils.Function in
  String.lowercase <@ String.substr_replace_all ~pattern:"Passes__" ~with_:""


let pass_of_module ~raise (module P : T) : pass * bool =
  let name = process_name P.name in
  let morphing =
    Morphing.morph
      ~compile:(P.compile ~raise)
      ~decompile:(P.decompile ~raise)
      ~reduction:(P.reduction ~raise)
  in
  { name; morphing }, P.is_enabled ()


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


let rec select_passes included name passes =
  match passes with
  | [] -> []
  | pass :: tl ->
    if String.equal name (fst pass).name
    then if included then [ pass ] else []
    else pass :: select_passes included name tl


(* executes a pass and check combined reductions *)
let compile_with_passes : type a. a -> a sub_pass list -> a =
 fun prg passes ->
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


let filter_before (passes : (pass * bool) list) stop_before =
  let included, stop = partial_pass_execution_from_name stop_before in
  if not (List.exists passes ~f:(fun (pass, _) -> String.equal stop pass.name))
  then failwith "No pass with the specified name";
  if List.exists passes ~f:(fun (pass, enabled) ->
         String.equal stop pass.name && not enabled)
  then failwith "A pass exist with the specified name but isn't enabled";
  select_passes included stop passes


let decompile_passes
    : type a.
      raise:raise_t -> ?stop_before:_ -> sort:a Selector.t -> (module T) list -> a -> a
  =
 fun ~raise ?stop_before ~sort passes value ->
  passes
  |> List.rev
  |> List.map ~f:(pass_of_module ~raise)
  |> fun default ->
  Option.value_map stop_before ~default ~f:(filter_before default)
  |> List.filter_map ~f:(fun (pass, enabled) -> if enabled then Some pass else None)
  |> List.map ~f:(fun (p : pass) -> p.morphing)
  |> List.map ~f:(Selector.select sort)
  |> List.fold ~init:value ~f:(fun prg pass -> pass.backward prg)


let compile_passes
    : type a.
      raise:raise_t -> ?stop_before:_ -> sort:a Selector.t -> (module T) list -> a -> a
  =
 fun ~raise ?stop_before ~sort passes prg ->
  passes
  |> List.map ~f:(pass_of_module ~raise)
  |> fun default ->
  Option.value_map stop_before ~default ~f:(filter_before default)
  |> List.filter_map ~f:(fun (pass, enabled) -> if enabled then Some pass else None)
  |> List.map ~f:(fun (p : pass) -> p.morphing)
  |> List.map ~f:(Selector.select sort)
  |> compile_with_passes prg
