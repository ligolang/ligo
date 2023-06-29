open! Memory_proto_pre_alpha
module List = Core.List
module Signature = Tezos_base.TzPervasives.Signature
module Data_encoding = Alpha_environment.Data_encoding
module MBytes = Bytes
module Error_monad = X_error_monad
module Proto_env = Tpe_016
open Error_monad
open Protocol


module Context_init = struct

  type account = {
      pkh : Signature.public_key_hash ;
      pk :  Signature.public_key ;
      sk :  Signature.secret_key ;
    }

  let generate_accounts n : (account * Alpha_context.Tez.t) list =
    let amount = Alpha_context.Tez.of_mutez_exn 4_000_000_000_000L in
    List.map ~f:(fun _ ->
        let (pkh, pk, sk) = Signature.generate_key () in
        let account = { pkh ; pk ; sk } in
        account, amount)
      (List.range 0 n)

  let make_shell
        ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.{
        level ;
        predecessor ;
        timestamp ;
        fitness ;
        operations_hash ;
        (* We don't care of the following values, only the shell validates them. *)
        proto_level = 0 ;
        validation_passes = 0 ;
        context = Alpha_environment.Context_hash.zero ;
    }

  let default_proof_of_work_nonce =
    MBytes.create Alpha_context.Constants.proof_of_work_nonce_size

  let protocol_param_key = [ "protocol_parameters" ]

  let check_constants_consistency (constants:Alpha_context.Constants.Parametric.t) : unit tzresult Lwt.t =
    let open Alpha_context.Constants in
    let open Error_monad in
    let open Lwt_result_syntax in
    let Parametric.({ blocks_per_cycle ; blocks_per_commitment ;
      blocks_per_stake_snapshot ; _ }) = constants in
    let* () = Error_monad.unless (blocks_per_commitment <= blocks_per_cycle)
      (fun () -> failwith "Inconsistent constants : blocks per commitment must be \
                           less than blocks per cycle")
    in
    let* () = Error_monad.unless (blocks_per_cycle >= blocks_per_stake_snapshot)
      (fun () -> failwith "Inconsistent constants : blocks per cycle \
                           must be superior than blocks per roll snapshot")
    in
    return ()

  let initial_context
        constants
        header
        commitments
        (initial_accounts : (account * Alpha_context.Tez.t) trace)
        security_deposit_ramp_up_cycles
        no_reward_cycles
    =
    let open Tezos_base.TzPervasives.Error_monad in
    let open Lwt_syntax in
    let open Lwt in
    let bootstrap_accounts =
      List.mapi ~f:(fun i ({ pk ; pkh ; _ }, amount) ->
          Alpha_context.Parameters.{ public_key_hash = pkh ; public_key = if i > 0 then None else Some pk ; amount ; delegate_to = None ; consensus_key = None }
        ) initial_accounts
    in
    let json =
      Data_encoding.Json.construct
        Alpha_context.Parameters.encoding
        Alpha_context.Parameters.{
          bootstrap_accounts ;
          bootstrap_contracts = [] ;
          commitments ;
          constants ;
          security_deposit_ramp_up_cycles ;
          no_reward_cycles ;
      }
    in
    let proto_params =
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json
    in
    let* ctxt = Tpenv.(
      Context.add Memory_context.empty ["version"] (MBytes.of_string "genesis")
      )
    in
    let* ctxt = Tpenv.Context.(
      add ctxt protocol_param_key proto_params
      )
    in
    let* context =
      let+ x = Main.init Proto_env.Chain_id.zero ctxt header in
      Alpha_environment.wrap_tzresult x
    in
    return context

  let genesis
        ?(commitments = [])
        ?(security_deposit_ramp_up_cycles = None)
        ?(no_reward_cycles = None)
        (initial_accounts : (account * Alpha_context.Tez.t) list)
    =
    let open Lwt_result_syntax in
    if initial_accounts = [] then
      Stdlib.failwith "Must have one account with a roll to bake";

    (* Check there is at least one roll *)
    let constants : Alpha_context.Constants.Parametric.t = Tp_016_params.Default_parameters.constants_test in
    let* () = check_constants_consistency constants in
    let hash =
      Alpha_environment.Block_hash.of_b58check_exn "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
    in
    let shell = make_shell
                  ~level:0l
                  ~predecessor:hash
                  ~timestamp:Tezos_base.TzPervasives.Time.Protocol.epoch
                  ~fitness:[]
                  ~operations_hash: Alpha_environment.Operation_list_list_hash.zero in
    let* context =
      initial_context
        constants
        shell
        commitments
        initial_accounts
        security_deposit_ramp_up_cycles
        no_reward_cycles
    in
    return (context, shell, hash)

  let init
        ?commitments
        n =
    let open Error_monad in
    let open Lwt_result_syntax in
    let accounts = generate_accounts n in
    let contracts = List.map ~f:(fun (a, _) ->
                        Alpha_context.Contract.Implicit (a.pkh)) accounts in
    let* ctxt =
      genesis
        ?commitments
        accounts
    in
    return (ctxt, accounts, contracts)

  let contents
        ~predecessor
        ?(proof_of_work_nonce = default_proof_of_work_nonce)
        ?(round = Alpha_context.Round.zero) ?seed_nonce_hash ?(liquidity_baking_toggle_vote = Liquidity_baking_repr.LB_off) () =
    let payload_hash = Alpha_context.Block_payload.hash ~predecessor_hash:predecessor ~payload_round:round [] in (* TODO: check if this is correct *)
    Alpha_context.Block_header.({
        payload_hash ;
        payload_round = round ;
        proof_of_work_nonce ;
        seed_nonce_hash ;
        liquidity_baking_toggle_vote ;
      })

  let begin_validation_and_application ctxt chain_id mode ~predecessor =
    let open Lwt_result_syntax in
    let* validation_state =
      Main.begin_validation ctxt chain_id mode ~predecessor
    in
    let* application_state =
      Main.begin_application ctxt chain_id mode ~predecessor
    in
    return (validation_state, application_state)

  let begin_construction ?(round=Alpha_context.Round.zero) ~timestamp ~(header:Alpha_context.Block_header.shell_header) ~hash ctxt =
    let (>>=) = Lwt_syntax.( let* ) in
    let (>>=?) = Lwt_result_syntax.( let* ) in
    let contents = contents ~round ~predecessor:hash () in
    let protocol_data =
      let open! Alpha_context.Block_header in {
        contents ;
        signature = Signature.zero ;
      } in
    begin_validation_and_application ctxt Alpha_environment.Chain_id.zero
      (Construction { predecessor_hash = hash ; timestamp ; block_header_data = protocol_data }) ~predecessor:header
      >>= fun x -> Lwt.return @@ Alpha_environment.wrap_tzresult x >>=? fun (_, state) ->
        Lwt_result_syntax.return state.ctxt

  let main n =
    let (>>=?) = Lwt_result_syntax.( let* ) in
    init n >>=? fun ((ctxt, header, hash), accounts, contracts) ->
    let timestamp = Environment.Time.of_seconds @@ 1645498950L in
    begin_construction ~timestamp ~header ~hash ctxt.context >>=? fun ctxt ->
      Lwt_result_syntax.return (ctxt, accounts, contracts)

end

type identity = {
    public_key_hash : Signature.public_key_hash;
    public_key : Signature.public_key;
    secret_key : Signature.secret_key;
    implicit_contract : Alpha_context.Contract.t;
  }


type environment = {
    tezos_context : Alpha_context.t ;
    identities : identity list ;
  }

let init_environment ?(n = 2) () =
  let open Lwt_result_syntax in
  let* (tezos_context, accounts, contracts) = Context_init.main n in
  let accounts = List.map ~f:fst accounts in
  let x = Memory_proto_pre_alpha.Protocol.Alpha_context.Gas.Arith.(integral_of_int_exn 800000) in
  let tezos_context = Alpha_context.Gas.set_limit tezos_context x in
  let identities =
    List.map ~f:(fun ((a:Context_init.account), c) -> {
                  public_key = a.pk ;
                  public_key_hash = a.pkh ;
                  secret_key = a.sk ;
                  implicit_contract = c ;
      }) @@
      List.zip_exn accounts contracts in
  return {tezos_context ; identities}

let dummy_environment_ : environment option ref = ref None

let dummy_environment () : environment =
  match ! dummy_environment_ with
  | None ->
     let dummy_environment = (X_error_monad.force_lwt ~msg:"Init_proto_alpha : initing dummy environment" @@
                                init_environment ()) in
     dummy_environment_ := Some dummy_environment ;
     dummy_environment
  | Some dummy_environment -> dummy_environment

let test_environment_ : environment option ref = ref None

let test_environment () =
  match ! test_environment_ with
  | None ->
     let test_environment = (X_error_monad.force_lwt ~msg:"Init_proto_alpha : initing dummy environment" @@
                               init_environment ~n:6 ()) in
     test_environment_ := Some test_environment ;
     test_environment
  | Some test_environment -> test_environment
