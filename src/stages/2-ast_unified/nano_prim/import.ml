module Ne_list = Simple_utils.Ne_list
module Value_var = Ligo_prim.Value_var
module Module_var = Ligo_prim.Module_var

type t =
  | Import_rename of
      { alias : Module_var.t
      ; module_path : Module_var.t Ne_list.t
      }
  | Import_all_as of
      { alias : Module_var.t
      ; module_str : string
      }
  | Import_selected of
      { imported : Value_var.t Ne_list.t
      ; module_str : string
      }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
