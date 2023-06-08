open Ligo_prim
module Ligo_string = Simple_utils.Ligo_string

type contract_pass_data =
  { contract_type : Helpers.contract_type
  ; main_name : Value_var.t
  ; module_path : Module_var.t list
  }
