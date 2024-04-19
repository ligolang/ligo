module Proto = Tezos_protocol_019_PtParisB
module Alpha_environment = Tezos_protocol_environment_019_PtParisB
module Raw_protocol = Tezos_raw_protocol_019_PtParisB
module Parameters = Tezos_protocol_019_PtParisB_parameters
module Client = Tezos_client_019_PtParisB
module Test_helpers = Tezos_019_PtParisB_test_helpers

(* Alcotezt redirects [Format.std_formatter] and [Format.err_formatter]
   to a buffer, we need to redirect them back to [stdout] and [stderr] *)

let redirect_formatter fmt ~oc =
  Format.pp_set_formatter_output_functions fmt (output_substring oc) (fun () -> flush oc)

let () =
  redirect_formatter ~oc:stdout Format.std_formatter;
  redirect_formatter ~oc:stderr Format.err_formatter

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult

module Alpha_error_monad = Alpha_environment.Error_monad
include Proto

let protocol_str = "parisb"
let protocol_def_str = "PARIS_B"
