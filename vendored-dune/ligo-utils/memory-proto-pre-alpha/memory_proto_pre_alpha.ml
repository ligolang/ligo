module Name = struct let name = "alpha" end
module Proto = Tezos_protocol_015_PtLimaPt
module Alpha_environment = Tp_environment_015_PtLimaPt
module Raw_protocol = Trp_015_PtLimaPt
module Parameters = Tp015_parameters
module Client = Tc_015
module Test_helpers = Tz015_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
