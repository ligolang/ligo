module Name = struct let name = "alpha" end
module Proto = Tp_016
module Alpha_environment = Tpe_016
module Raw_protocol = Trp_016
module Parameters = Tp016_parameters
module Client = Tc_016
module Test_helpers = Tz016_test_helpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
