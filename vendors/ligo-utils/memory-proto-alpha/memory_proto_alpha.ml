module Name = struct let name = "alpha" end
module Proto = Tezos_protocol_015_PtLimaPt
module Alpha_environment = Tp_environment_015_PtLimaPt
module Raw_protocol = Tezos_raw_protocol_015_PtLimaPt
module Parameters = Tezos_protocol_015_PtLimaPt_parameters
module Client = Tezos_client_015_PtLimaPt
module Test_helpers = Tz015_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
