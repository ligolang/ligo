module Name = struct let name = "alpha" end
module Proto = Tezos_protocol_014_PtKathma
module Alpha_environment = Tp_environment_014_PtKathma
module Raw_protocol = Tezos_raw_protocol_014_PtKathma
module Parameters = Tp014_parameters
module Client = Tezos_client_014_PtKathma
module Test_helpers = P014_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
