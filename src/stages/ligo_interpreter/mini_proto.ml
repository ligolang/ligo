open Proto_alpha_utils.Memory_proto_alpha.Protocol
open Alpha_context

type contract = int
type addr = Counter of contract
module StateMap = Map.Make(struct type t = addr let compare (Counter a) (Counter b) = Int.compare a b end)


type script = {
    code :  Types.value ;
    storage : Types.value ;
  }

type state = {
    script : script ;
    script_balance : Tez.t ;
  }
type state_map = state StateMap.t

type step_constants = {
    source : contract ;
    payer : contract ;
    self : contract ;
    amount : Tez.t ;
    balance : Tez.t ;
    chain_id : bytes ;
    now : Script_timestamp.t ;
    origination_counter : int ; (*used to generate fake address for originated contracts *)
  }

type t = {
    contracts : state_map ;
    step_constants : step_constants ;
  }

let option_to_context : Proto_alpha_utils.Memory_proto_alpha.options -> t =
  fun {tezos_context=_TODO;source;payer;self;amount;chain_id=_;balance;now} ->
    ignore source ; ignore payer; ignore self ;
    {
      contracts = StateMap.empty ;
      step_constants = { source=0 ; payer=0 ; self=0 ; amount ; balance ; now ; chain_id = Bytes.empty ; origination_counter = 0 }
    }
