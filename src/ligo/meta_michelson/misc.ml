module Signature = Tezos_base.TzPervasives.Signature
open Tezos_utils.Memory_proto_alpha
module Data_encoding = Alpha_environment.Data_encoding
module MBytes = Alpha_environment.MBytes
module Error_monad = Tezos_utils.Error_monad
open Error_monad

module Context_init = struct

  type account = {
      pkh : Signature.Public_key_hash.t ;
      pk :  Signature.Public_key.t ;
      sk :  Signature.Secret_key.t ;
    }

  let generate_accounts n : (account * Tez_repr.t) list =
    let amount = Tez_repr.of_mutez_exn 4_000_000_000_000L in
    List.map (fun _ ->
        let (pkh, pk, sk) = Signature.generate_key () in
        let account = { pkh ; pk ; sk } in
        account, amount)
      (Tezos_utils.List.range n)

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

  let check_constants_consistency constants =
    let open Constants_repr in
    let open Error_monad in
    let { blocks_per_cycle ; blocks_per_commitment ;
          blocks_per_roll_snapshot ; _ } = constants in
    Error_monad.unless (blocks_per_commitment <= blocks_per_cycle)
      (fun () -> failwith "Inconsistent constants : blocks per commitment must be \
                           less than blocks per cycle") >>=? fun () ->
    Error_monad.unless (blocks_per_cycle >= blocks_per_roll_snapshot)
      (fun () -> failwith "Inconsistent constants : blocks per cycle \
                           must be superior than blocks per roll snapshot") >>=?
      return


  let initial_context
        constants
        header
        commitments
        initial_accounts
        security_deposit_ramp_up_cycles
        no_reward_cycles
    =
    let open Tezos_base.TzPervasives.Error_monad in
    let bootstrap_accounts =
      List.map (fun ({ pk ; pkh ; _ }, amount) ->
          Parameters_repr.{ public_key_hash = pkh ; public_key = Some pk ; amount }
        ) initial_accounts
    in
    let json =
      Data_encoding.Json.construct
        Parameters_repr.encoding
        Parameters_repr.{
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
    Tezos_protocol_environment_memory.Context.(
      set empty ["version"] (MBytes.of_string "genesis")
    ) >>= fun ctxt ->
    Tezos_protocol_environment_memory.Context.(
      set ctxt protocol_param_key proto_params
    ) >>= fun ctxt ->
    Main.init ctxt header
    >|= Alpha_environment.wrap_error >>=? fun { context; _ } ->
    return context

  let genesis
        ?(preserved_cycles = Constants_repr.default.preserved_cycles)
        ?(blocks_per_cycle = Constants_repr.default.blocks_per_cycle)
        ?(blocks_per_commitment = Constants_repr.default.blocks_per_commitment)
        ?(blocks_per_roll_snapshot = Constants_repr.default.blocks_per_roll_snapshot)
        ?(blocks_per_voting_period = Constants_repr.default.blocks_per_voting_period)
        ?(time_between_blocks = Constants_repr.default.time_between_blocks)
        ?(endorsers_per_block = Constants_repr.default.endorsers_per_block)
        ?(hard_gas_limit_per_operation = Constants_repr.default.hard_gas_limit_per_operation)
        ?(hard_gas_limit_per_block = Constants_repr.default.hard_gas_limit_per_block)
        ?(proof_of_work_threshold = Int64.(neg one))
        ?(tokens_per_roll = Constants_repr.default.tokens_per_roll)
        ?(michelson_maximum_type_size = Constants_repr.default.michelson_maximum_type_size)
        ?(seed_nonce_revelation_tip = Constants_repr.default.seed_nonce_revelation_tip)
        ?(origination_size = Constants_repr.default.origination_size)
        ?(block_security_deposit = Constants_repr.default.block_security_deposit)
        ?(endorsement_security_deposit = Constants_repr.default.endorsement_security_deposit)
        ?(block_reward = Constants_repr.default.block_reward)
        ?(endorsement_reward = Constants_repr.default.endorsement_reward)
        ?(cost_per_byte = Constants_repr.default.cost_per_byte)
        ?(hard_storage_limit_per_operation = Constants_repr.default.hard_storage_limit_per_operation)
        ?(commitments = [])
        ?(security_deposit_ramp_up_cycles = None)
        ?(no_reward_cycles = None)
        (initial_accounts : (account * Tez_repr.t) list)
    =
    if initial_accounts = [] then
      Pervasives.failwith "Must have one account with a roll to bake";

    (* Check there is at least one roll *)
    let open Tezos_base.TzPervasives.Error_monad in
    begin try
        let (>>?=) x y = match x with
          | Ok(a) -> y a
          | Error(b) -> fail @@ List.hd b in
        fold_left_s (fun acc (_, amount) ->
            Alpha_environment.wrap_error @@
              Tez_repr.(+?) acc amount >>?= fun acc ->
                                            if acc >= tokens_per_roll then
                                              raise Exit
                                            else return acc
          ) Tez_repr.zero initial_accounts >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll"
    with Exit -> return ()
    end >>=? fun () ->

    let constants : Constants_repr.parametric = {
        preserved_cycles ;
        blocks_per_cycle ;
        blocks_per_commitment ;
        blocks_per_roll_snapshot ;
        blocks_per_voting_period ;
        time_between_blocks ;
        endorsers_per_block ;
        hard_gas_limit_per_operation ;
        hard_gas_limit_per_block ;
        proof_of_work_threshold ;
        tokens_per_roll ;
        michelson_maximum_type_size ;
        seed_nonce_revelation_tip ;
        origination_size ;
        block_security_deposit ;
        endorsement_security_deposit ;
        block_reward ;
        endorsement_reward ;
        cost_per_byte ;
        hard_storage_limit_per_operation ;
      } in
    check_constants_consistency constants >>=? fun () ->

    let hash =
      Alpha_environment.Block_hash.of_b58check_exn "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
    in
    let shell = make_shell
                  ~level:0l
                  ~predecessor:hash
                  ~timestamp:Tezos_utils.Time.epoch
                  ~fitness: (Fitness_repr.from_int64 0L)
                  ~operations_hash: Alpha_environment.Operation_list_list_hash.zero in
    initial_context
      constants
      shell
      commitments
      initial_accounts
      security_deposit_ramp_up_cycles
      no_reward_cycles
    >>=? fun context ->
    return (context, shell, hash)

  let init
        ?(slow=false)
        ?preserved_cycles
        ?endorsers_per_block
        ?commitments
        n =
    let open Error_monad in
    let accounts = generate_accounts n in
    let contracts = List.map (fun (a, _) ->
                        Alpha_context.Contract.implicit_contract (a.pkh)) accounts in
    begin
      if slow then
        genesis
          ?preserved_cycles
          ?endorsers_per_block
          ?commitments
          accounts
      else
        genesis
          ?preserved_cycles
          ~blocks_per_cycle:32l
          ~blocks_per_commitment:4l
          ~blocks_per_roll_snapshot:8l
          ~blocks_per_voting_period:(Int32.mul 32l 8l)
          ?endorsers_per_block
          ?commitments
          accounts
    end >>=? fun ctxt ->
    return (ctxt, accounts, contracts)

  let contents
        ?(proof_of_work_nonce = default_proof_of_work_nonce)
        ?(priority = 0) ?seed_nonce_hash () =
    Alpha_context.Block_header.({
      priority ;
      proof_of_work_nonce ;
      seed_nonce_hash ;
    })


  let begin_construction ?(priority=0) ~timestamp ~(header:Alpha_context.Block_header.shell_header) ~hash ctxt =
    let contents = contents ~priority () in
    let protocol_data = Alpha_context.Block_header.{
        contents ;
        signature = Signature.zero ;
      } in
    let header = {
        Alpha_context.Block_header.shell = {
          predecessor = hash ;
          proto_level = header.proto_level ;
          validation_passes = header.validation_passes ;
          fitness = header.fitness ;
          timestamp ;
          level = header.level ;
          context = Alpha_environment.Context_hash.zero ;
          operations_hash = Alpha_environment.Operation_list_list_hash.zero ;
        } ;
        protocol_data = {
            contents ;
            signature = Signature.zero ;
          } ;
      } in
    Main.begin_construction
      ~chain_id: Alpha_environment.Chain_id.zero
      ~predecessor_context: ctxt
      ~predecessor_timestamp: header.shell.timestamp
      ~predecessor_fitness: header.shell.fitness
      ~predecessor_level: header.shell.level
      ~predecessor:hash
      ~timestamp
      ~protocol_data
      () >>= fun x -> Lwt.return @@ Alpha_environment.wrap_error x >>=? fun state ->
    return state.ctxt

  let main n =
    init n >>=? fun ((ctxt, header, hash), accounts, contracts) ->
    let timestamp = Tezos_base.Time.now () in
    begin_construction ~timestamp ~header ~hash ctxt >>=? fun ctxt ->
    return (ctxt, accounts, contracts)

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

let init_environment () =
  Context_init.main 10 >>=? fun (tezos_context, accounts, contracts) ->
  let accounts = List.map fst accounts in
  let tezos_context = Alpha_context.Gas.set_limit tezos_context @@ Z.of_int 350000 in
  let identities =
    List.map (fun ((a:Context_init.account), c) -> {
          public_key = a.pk ;
          public_key_hash = a.pkh ;
          secret_key = a.sk ;
          implicit_contract = c ;
        }) @@
    List.combine accounts contracts in
  return {tezos_context ; identities}

let contextualize ~msg ?environment f =
  let lwt =
    let environment = match environment with
      | None -> init_environment ()
      | Some x -> return x in
    environment >>=? f
  in
  force_ok ~msg @@ Lwt_main.run lwt
