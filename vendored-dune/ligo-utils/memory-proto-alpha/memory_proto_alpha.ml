(* module Name = struct let name = "alpha" end *)
module Proto = Tezos_protocol_016_PtMumbai
module Alpha_environment = Tp_environment_016_PtMumbai
module Raw_protocol = Trp_016
module Parameters = Tp_016__params
module Client = Tc_016
module Test_helpers = Tezos_016_PtMumbai_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
