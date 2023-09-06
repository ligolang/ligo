module Name = struct let name = "alpha" end
module Proto = Tezos_protocol_018_Proxford
module Alpha_environment = Tezos_protocol_environment_018_Proxford
module Raw_protocol = Tezos_raw_protocol_018_Proxford
module Parameters = Tezos_protocol_018_Proxford_parameters
module Client = Tezos_client_018_Proxford
module Test_helpers = Tezos_018_Proxford_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
