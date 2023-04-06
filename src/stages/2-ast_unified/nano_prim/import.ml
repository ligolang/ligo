type t =
  | Import_rename of
      { alias : Ligo_prim.Module_var.t
      ; module_path : Ligo_prim.Module_var.t Simple_utils.List.Ne.t
      }
  | Import_all_as of
      { alias : Ligo_prim.Module_var.t
      ; module_str : string
      }
  | Import_selected of
      { imported : Ligo_prim.Value_var.t Simple_utils.List.Ne.t
      ; module_str : string
      }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]
