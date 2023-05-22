(* module Name = struct let name = "alpha" end *)
module Proto = Tp_017
module Alpha_environment = Tpe_017
module Raw_protocol = Trp_017
module Parameters = Tp_017_params
module Client = Tc_017
module Test_helpers = Tz17_testhelpers

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
include Proto
