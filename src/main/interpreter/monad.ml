open Trace
(*
  That monad do not seem very useful now,
  but it could become useful if we want to support multiple testing mode (against node, or memory)
*)


module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators
module Int_repr = Ligo_interpreter.Int_repr_copied
module Exc = Ligo_interpreter_exc

type execution_trace = unit
type 'a result_monad = ('a,Errors.interpreter_error) result

let ( let>>= ) o f = Trace.bind f o

let corner_case ?(loc = Location.generated) () = Errors.generic_error loc "Corner case, please report to devs." 

let wrap_compare compare a b =
  let res = compare a b in
  if (res = 0) then 0 else if (res > 0) then 1 else -1

module Command = struct
  type 'a t =
    | Reset_state : Location.t * LT.value * LT.value -> unit t
    | External_call : Location.t * LT.value * LT.value * LT.value -> Tezos_state.state_error option t
    | State_error_to_value : Tezos_state.state_error -> LT.value t
    | Get_storage : Location.t * LT.value -> LT.value t
    | Get_balance : Location.t * LT.value -> LT.value t
    | Get_last_originations : unit -> LT.value t
    | Compile_expression : Location.t * LT.value * string * string * LT.value option -> LT.value t
    | Compile_contract : string * string -> (LT.value * LT.value) t
    | Compile_meta_value : Location.t * LT.value -> LT.value t
    | Inject_script : Location.t * LT.value * LT.value -> LT.value t
    | Set_now : Location.t * Z.t -> unit t
    | Set_source : LT.value -> unit t
    | Set_baker : LT.value -> unit t
    | Get_bootstrap : Location.t * LT.value -> LT.value t
    | Michelson_equal : Location.t * LT.value * LT.value -> bool t
    | Int_compare_wrapped : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_compare : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_abs : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_of_zint : Z.t -> Int_repr.z Int_repr.num t
    | Int_to_zint : 'a Int_repr.num -> Z.t t
    | Int_of_int64 : int64 -> Int_repr.z Int_repr.num t
    | Int_to_int64 : _ Int_repr.num -> int64 option t
    | Int_is_nat : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num option t
    | Int_neg : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_mul : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_mul_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_ediv :
      _ Int_repr.num * _ Int_repr.num ->
      (Int_repr.z Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_ediv_n :
      Int_repr.n Int_repr.num * Int_repr.n Int_repr.num ->
      (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_sub : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_shift_left : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_shift_right : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_logor : ('a Int_repr.num * 'a Int_repr.num) -> 'a Int_repr.num t
    | Int_logand : (_ Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_logxor : (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_lognot : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_of_int : int -> Int_repr.z Int_repr.num t
    | Int_int : Int_repr.n Int_repr.num -> Int_repr.z Int_repr.num t

  let eval
    : type a.
      a t ->
      Tezos_state.context ->
      execution_trace ref option ->
      (a * Tezos_state.context) result_monad
    = fun command ctxt _log ->
    match command with
    | Reset_state (loc,n,amts) ->
      let* amts = trace_option (corner_case ()) @@ LC.get_list amts in
      let* amts = bind_map_list
        (fun x ->
          let* x = trace_option (corner_case ()) @@ LC.get_nat x in
          ok (Z.to_int64 x) )
        amts
      in
      let* n = trace_option (corner_case ()) @@ LC.get_nat n in
      let* ctxt = Tezos_state.init_ctxt ~loc ~initial_balances:amts ~n:(Z.to_int n) () in
      ok ((),ctxt)
    | External_call (loc, addr, param, amt) -> (
      match addr, param , amt with
      | V_Ct ( C_address dst) , V_Michelson (Ty_code (param,_,_)), V_Ct ( C_nat amt ) -> (
        let* x = Tezos_state.transfer ~loc ctxt dst param amt in 
        match x with
        | Success ctxt -> ok (None, ctxt)
        | Fail errs -> ok (Some errs, ctxt)
      )
      | _ -> failwith "should have been caught by the typer" 
    )
    | State_error_to_value errs -> (
      match Tezos_state.get_contract_rejection_data errs with
      | Some (addr,v) ->
        let t = Michelson_backend.storage_retreival_dummy_ty in
        let v = LT.V_Michelson (Ty_code (v, t, Ast_typed.t_unit ())) in
        let addr = LT.V_Ct (C_address addr) in
        let err = LC.v_ctor "Rejected" (LC.v_pair (v,addr)) in
        ok (LC.v_ctor "Fail" err, ctxt)
      | None ->
        ok (LC.v_ctor "Fail" (LC.v_ctor "Other" (LC.v_unit ())), ctxt)
    )
    | Get_storage (loc, addr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* (storage',ty) = Tezos_state.get_storage ~loc ctxt addr in
      let storage = storage'
        |> Tezos_protocol_008_PtEdo2Zk.Protocol.Michelson_v1_primitives.strings_of_prims
        |> Tezos_micheline.Micheline.inject_locations (fun _ -> ())
      in
      (* TODO : find a way to get the type instead of t_unit:
       - current implem restrict michelson expression gotten from a Test.get_storage to be used in Test.compile_expr_subst*)
      let ret = LT.V_Michelson (Ty_code (storage,ty,Ast_typed.t_unit ())) in
      ok (ret, ctxt)
    | Get_balance (loc,addr) ->
      let* addr = trace_option (corner_case ()) @@ LC.get_address addr in
      let* balance = Tezos_state.get_balance ~loc ctxt addr in
      let mutez = Michelson_backend.int_of_mutez balance in
      let mich_data = let open Tezos_utils.Michelson in (int mutez, prim "mutez" , Ast_typed.t_mutez ()) in
      let balance = LT.V_Michelson (Ty_code mich_data) in
      ok (balance, ctxt)
    | Compile_expression (loc, source_file, syntax, exp_as_string, subst_opt) ->
      let* file_opt = trace_option (corner_case ()) @@ LC.get_string_option source_file in
      let* substs =
        match subst_opt with
        | None -> ok []
        | Some substs ->
          let* lst = trace_option (corner_case ()) @@ LC.get_list substs in
          let aux = fun el ->
            let* (s,c) = trace_option (corner_case ()) @@ LC.get_pair el in
            let* s = trace_option (corner_case ()) @@ LC.get_string s in
            let* i = trace_option (corner_case ()) @@ LC.get_michelson_expr c in
            ok (s, i)
          in
          bind_map_list aux lst
      in
      let aux = fun exp_str (s,_) -> (* TODO: a bit naive .. *)
        Str.substitute_first (Str.regexp ("\\$"^s)) (fun _ -> Michelson_backend.subst_vname s) exp_str
      in
      let exp_as_string' = List.fold_left aux exp_as_string substs in
      let* (mich_v, mich_ty, object_ty) = Michelson_backend.compile_expression ~loc syntax exp_as_string' file_opt substs in
      ok (LT.V_Michelson (LT.Ty_code (mich_v, mich_ty, object_ty)), ctxt)
    | Compile_meta_value (loc,x) ->
      let* x = Michelson_backend.compile_simple_val ~loc x in
      ok (LT.V_Michelson (LT.Ty_code x), ctxt)
    | Compile_contract (source_file, entrypoint) ->
      let* contract_code = Michelson_backend.compile_contract source_file entrypoint in
      let* size =
        let* s = Ligo_compile.Of_michelson.measure contract_code in
        ok @@ LT.V_Ct (C_int (Z.of_int s))
      in
      let contract = LT.V_Michelson (LT.Contract contract_code) in
      ok ((contract,size), ctxt)
    | Inject_script (loc, code, storage) -> (
      let* contract_code = trace_option (corner_case ()) @@ LC.get_michelson_contract code in
      let* (storage,_,_) = trace_option (corner_case ()) @@ LC.get_michelson_expr storage in
      let* (contract, res) = Tezos_state.originate_contract ~loc ctxt contract_code storage in
      match res with
      | Tezos_state.Success ctxt ->
        let addr = LT.V_Ct ( C_address contract ) in
        ok (addr, ctxt)
      | Tezos_state.Fail errs -> raise (Exc.Object_lang_ex (loc,errs))
    )
    | Set_now (loc, now) ->
      let* ctxt = Tezos_state.set_timestamp ~loc ctxt now in
      ok ((), ctxt) 
    | Set_source source ->
      let* source = trace_option (corner_case ()) @@ LC.get_address source in
      ok ((), {ctxt with source })
    | Set_baker baker ->
      let* baker = trace_option (corner_case ()) @@ LC.get_address baker in
      ok ((), {ctxt with baker })
    | Get_bootstrap (loc,x) -> (
      let* x = trace_option (corner_case ()) @@ LC.get_int x in
      match List.nth_opt ctxt.bootstrapped (Z.to_int x) with
      | Some x -> ok (LT.V_Ct (C_address x), ctxt)
      | None -> fail (Errors.generic_error loc "This bootstrap account do not exist")
    )
    | Michelson_equal (loc,a,b) ->
      let* (a,_,_) = trace_option (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr a in
      let* (b,_,_) = trace_option (Errors.generic_error loc "Can't compare contracts") @@
        LC.get_michelson_expr b in
      ok ((a=b), ctxt)
    | Get_last_originations () ->
      let aux (src, lst) =
        let src = LC.v_address src in
        let lst = LT.V_List (List.map LC.v_address lst) in
        (src, lst)
      in
      let v = LT.V_Map (List.map aux ctxt.last_originations) in
      ok (v,ctxt)
    | Int_compare_wrapped (x, y) ->
      ok (wrap_compare Int_repr.compare x y, ctxt)
    | Int_compare (x, y) -> ok (Int_repr.compare x y, ctxt)
    | Int_abs z -> ok (Int_repr.abs z, ctxt)
    | Int_of_int i -> ok (Int_repr.of_int i, ctxt)
    | Int_of_zint z -> ok (Int_repr.of_zint z, ctxt)
    | Int_to_zint z -> ok (Int_repr.to_zint z, ctxt)
    | Int_of_int64 i -> ok (Int_repr.of_int64 i, ctxt)
    | Int_to_int64 i -> ok (Int_repr.to_int64 i, ctxt)
    | Int_is_nat z -> ok (Int_repr.is_nat z, ctxt)
    | Int_neg n -> ok (Int_repr.neg n, ctxt)
    | Int_add (x, y) -> ok (Int_repr.add x y, ctxt)
    | Int_add_n (x, y) -> ok (Int_repr.add_n x y, ctxt)
    | Int_mul (x, y) -> ok (Int_repr.mul x y, ctxt)
    | Int_mul_n (x, y) -> ok (Int_repr.mul_n x y, ctxt)
    | Int_ediv (x, y) -> ok (Int_repr.ediv x y, ctxt)
    | Int_ediv_n (x, y) -> ok (Int_repr.ediv_n x y, ctxt)
    | Int_sub (x, y) -> ok (Int_repr.sub x y, ctxt)
    | Int_shift_left (x, y) -> ok (Int_repr.shift_left x y, ctxt)
    | Int_shift_right (x, y) -> ok (Int_repr.shift_right x y, ctxt)
    | Int_logor (x, y) -> ok (Int_repr.logor x y, ctxt)
    | Int_logand (x, y) -> ok (Int_repr.logand x y, ctxt)
    | Int_logxor (x, y) -> ok (Int_repr.logxor x y, ctxt)
    | Int_lognot n -> ok (Int_repr.lognot n, ctxt)
    | Int_int n -> ok (Int_repr.int n, ctxt)
end

type 'a t =
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Call : 'a Command.t -> 'a t
  | Return : 'a -> 'a t
  | Fail_ligo : Errors.interpreter_error -> 'a t

let rec eval
  : type a.
    a t ->
    Tezos_state.context ->
    execution_trace ref option ->
    (a * Tezos_state.context) result_monad
  = fun e ctxt log ->
  match e with
  | Bind (e', f) ->
    let>>= (v, ctxt) = eval e' ctxt log in
    eval (f v) ctxt log
  | Call command -> Command.eval command ctxt log
  | Return v -> ok (v, ctxt)
  | Fail_ligo err -> fail err

let fail err : 'a t = Fail_ligo err
let return (x: 'a) : 'a t = Return x
let call (command : 'a Command.t) : 'a t = Call command
let ( let>> ) o f = Bind (call o, f)
let ( let* ) o f = Bind (o, f)

let rec bind_list = function
  | [] -> return []
  | hd::tl ->
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ hd :: tl

let bind_map_list f lst = bind_list (List.map f lst)

let bind_fold_list f init lst =
  let aux x y =
    let* x = x in
    f x y
  in
  List.fold_left aux (return init) lst
