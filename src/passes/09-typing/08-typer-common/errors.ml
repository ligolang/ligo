open Simple_utils.Display


let stage = "typer"

type typer_error = [
  | `Typer_michelson_comb_no_record of Location.t
  | `Typer_michelson_comb_no_variant of Location.t
  | `Typer_unbound_type_variable of Ast_typed.Environment.t * Ast_typed.type_variable * Location.t
  | `Typer_unbound_variable of Ast_typed.Environment.t * Ast_typed.expression_variable * Location.t
  | `Typer_match_missing_case of Ast_core.label list * Ast_core.label list * Location.t
  | `Typer_match_extra_case of Ast_core.label list * Ast_core.label list * Location.t
  | `Typer_unbound_constructor of Ast_typed.Environment.t * Ast_core.label * Location.t
  | `Typer_redundant_constructor of Ast_typed.Environment.t * Ast_core.label * Location.t
  | `Typer_type_constant_wrong_number_of_arguments of Ast_core.type_constant * int * int * Location.t
  | `Typer_michelson_or_no_annotation of Ast_core.label * Location.t
  | `Typer_program_tracer of Ast_core.program * typer_error
  | `Typer_constant_declaration_tracer of Ast_core.expression_variable * Ast_core.expression * (Ast_typed.type_expression option) * typer_error
  | `Typer_match_error of Ast_core.matching_expr * Ast_typed.type_expression * Location.t
  | `Typer_needs_annotation of Ast_core.expression * string
  | `Typer_fvs_in_create_contract_lambda of Ast_core.expression * Ast_typed.expression_variable
  | `Typer_create_contract_lambda of Ast_core.constant' * Ast_core.expression
  | `Typer_should_be_a_function_type of Ast_typed.type_expression * Ast_core.expression
  | `Typer_bad_record_access of Ast_core.label * Ast_core.expression * Ast_typed.type_expression * Location.t
  | `Typer_expression_tracer of Ast_core.expression * typer_error
  | `Typer_record_access_tracer of Ast_typed.expression * typer_error
  | `Typer_assert_equal of Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_corner_case of string
  | `Typer_bad_collect_loop of Ast_typed.type_expression * Location.t
  | `Typer_declaration_order_record of Location.t
  | `Typer_too_small_record of Location.t
  | `Typer_expected_record of Ast_typed.type_expression
  | `Typer_expected_variant of Ast_typed.type_expression
  | `Typer_wrong_param_number of string * int * Ast_typed.type_expression list
  | `Typer_expected_function of Ast_typed.type_expression
  | `Typer_expected_pair of Ast_typed.type_expression
  | `Typer_expected_list of Ast_typed.type_expression
  | `Typer_expected_set of Ast_typed.type_expression
  | `Typer_expected_map of Ast_typed.type_expression
  | `Typer_expected_big_map of Ast_typed.type_expression
  | `Typer_expected_option of Ast_typed.type_expression
  | `Typer_expected_nat of Ast_typed.type_expression
  | `Typer_expected_bytes of Ast_typed.type_expression
  | `Typer_expected_key of Ast_typed.type_expression
  | `Typer_expected_signature of Ast_typed.type_expression
  | `Typer_expected_contract of Ast_typed.type_expression
  | `Typer_expected_string of Ast_typed.type_expression
  | `Typer_expected_key_hash of Ast_typed.type_expression
  | `Typer_expected_mutez of Ast_typed.type_expression
  | `Typer_expected_op_list of Ast_typed.type_expression
  | `Typer_expected_int of Ast_typed.type_expression
  | `Typer_expected_bool of Ast_typed.type_expression
  | `Typer_not_matching of Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_not_annotated of Location.t
  | `Typer_bad_substraction
  | `Typer_wrong_size of Ast_typed.type_expression
  | `Typer_wrong_neg of Ast_typed.type_expression
  | `Typer_wrong_not of Ast_typed.type_expression
  | `Typer_typeclass_error of Ast_typed.type_expression list list * Ast_typed.type_expression list
  | `Typer_converter of Ast_typed.type_expression
  | `Typer_uncomparable_types of Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_comparator_composed of Ast_typed.type_expression
  | `Typer_constant_decl_tracer of Ast_core.expression_variable * Ast_core.expression * Ast_typed.type_expression option * typer_error
  | `Typer_match_variant_tracer of Ast_core.matching_expr * typer_error
  | `Typer_unrecognized_type_constant of Ast_core.type_expression
  | `Typer_expected_ascription of Ast_core.expression
  | `Typer_different_types of Ast_typed.type_expression * Ast_typed.type_expression
]

let michelson_comb_no_record (loc:Location.t) = `Typer_michelson_comb_no_record loc
let michelson_comb_no_variant (loc:Location.t) = `Typer_michelson_comb_no_variant loc
let unbound_type_variable (e:Ast_typed.Environment.t) (tv:Ast_typed.type_variable) (loc:Location.t) = `Typer_unbound_type_variable (e,tv,loc)
let unbound_variable (e:Ast_typed.Environment.t) (v:Ast_typed.expression_variable) (loc:Location.t) = `Typer_unbound_variable (e,v,loc)
let match_missing_case (m:Ast_core.label list) (v:Ast_core.label list) (loc:Location.t) = `Typer_match_missing_case (m, v, loc)
let match_extra_case (m:Ast_core.label list) (v:Ast_core.label list) (loc:Location.t) = `Typer_match_extra_case (m, v, loc)
let unbound_constructor (e:Ast_typed.Environment.t) (c:Ast_core.label) (loc:Location.t) = `Typer_unbound_constructor (e,c,loc)
let type_constant_wrong_number_of_arguments (op:Ast_core.type_constant) (expected:int) (actual:int) loc = `Typer_type_constant_wrong_number_of_arguments (op,expected,actual,loc)
let redundant_constructor (e:Ast_typed.Environment.t) (c:Ast_core.label) (loc:Location.t) = `Typer_redundant_constructor (e,c,loc)
let michelson_or (c:Ast_core.label) (loc:Location.t) = `Typer_michelson_or_no_annotation (c,loc)
let program_error_tracer (p:Ast_core.program) (err:typer_error) = `Typer_program_tracer (p,err)
let constant_declaration_error_tracer (name:Ast_core.expression_variable) (ae:Ast_core.expression) (expected: Ast_typed.type_expression option) (err:typer_error) =
  `Typer_constant_declaration_tracer (name,ae,expected,err)
let match_error ~(expected: Ast_core.matching_expr) ~(actual: Ast_typed.type_expression) (loc:Location.t) =
  `Typer_match_error (expected,actual,loc)
let needs_annotation (e:Ast_core.expression) (case:string) = `Typer_needs_annotation (e,case)
let fvs_in_create_contract_lambda (e:Ast_core.expression) (fvar:Ast_typed.expression_variable) = `Typer_fvs_in_create_contract_lambda (e,fvar)
let create_contract_lambda (cst : Ast_core.constant') (e : Ast_core.expression) = `Typer_create_contract_lambda (cst,e)
let type_error_approximate ~(actual: Ast_typed.type_expression) ~(expression:Ast_core.expression) =
  `Typer_should_be_a_function_type (actual,expression)
let bad_record_access (field:Ast_core.label) (ae:Ast_core.expression) (t:Ast_typed.type_expression) (loc:Location.t) =
  `Typer_bad_record_access (field,ae,t,loc)
let expression_tracer ae err = `Typer_expression_tracer (ae,err)
let record_access_tracer (e:Ast_typed.expression) (err:typer_error) = `Typer_record_access_tracer (e,err)
let assert_equal (expected:Ast_typed.type_expression) (actual:Ast_typed.type_expression) = `Typer_assert_equal (expected,actual)
let corner_case desc = `Typer_corner_case desc
let bad_collect_loop (t:Ast_typed.type_expression) (loc:Location.t) = `Typer_bad_collect_loop (t,loc)
let declaration_order_record (loc:Location.t) = `Typer_declaration_order_record loc
let too_small_record (loc:Location.t) = `Typer_too_small_record loc
let expected_record (t:Ast_typed.type_expression) = `Typer_expected_record t
let expected_variant (t:Ast_typed.type_expression) = `Typer_expected_variant t
let wrong_param_number (name:string) (expected:int) (actual:Ast_typed.type_expression list) =
  `Typer_wrong_param_number (name,expected,actual)
let bad_list_fold_tracer err = `Typer_bad_list_fold_tracer err
let bad_set_fold_tracer err = `Typer_bad_set_fold_tracer err
let bad_map_fold_tracer err = `Typer_bad_map_fold_tracer err
let expected_function (t:Ast_typed.type_expression) = `Typer_expected_function t
let expected_pair (t:Ast_typed.type_expression) = `Typer_expected_pair t
let expected_list (t:Ast_typed.type_expression) = `Typer_expected_list t
let expected_set (t:Ast_typed.type_expression) = `Typer_expected_set t
let expected_map (t:Ast_typed.type_expression) = `Typer_expected_map t
let expected_big_map (t:Ast_typed.type_expression) = `Typer_expected_big_map t
let expected_option (t:Ast_typed.type_expression) = `Typer_expected_option t
let expected_nat (t:Ast_typed.type_expression) = `Typer_expected_nat t
let expected_bytes (t:Ast_typed.type_expression) = `Typer_expected_bytes t
let expected_key (t:Ast_typed.type_expression) = `Typer_expected_key t
let expected_signature (t:Ast_typed.type_expression) = `Typer_expected_signature t
let expected_contract (t:Ast_typed.type_expression) = `Typer_expected_contract t
let expected_string (t:Ast_typed.type_expression) = `Typer_expected_string t
let expected_key_hash (t:Ast_typed.type_expression) = `Typer_expected_key_hash t
let expected_mutez (t:Ast_typed.type_expression) = `Typer_expected_mutez t
let expected_op_list (t:Ast_typed.type_expression) = `Typer_expected_op_list t
let expected_int (t:Ast_typed.type_expression) = `Typer_expected_int t
let expected_bool (t:Ast_typed.type_expression) = `Typer_expected_bool t
let expected_ascription (t:Ast_core.expression) = `Typer_expected_ascription t
let not_matching (t1:Ast_typed.type_expression) (t2:Ast_typed.type_expression) = `Typer_not_matching (t1,t2)
let not_annotated (loc: Location.t) = `Typer_not_annotated loc
let bad_substraction = `Typer_bad_substraction
let wrong_size (t:Ast_typed.type_expression) = `Typer_wrong_size t
let wrong_neg (t:Ast_typed.type_expression) = `Typer_wrong_neg t
let wrong_not (t:Ast_typed.type_expression) = `Typer_wrong_not t
let typeclass_error (exps:Ast_typed.type_expression list list) (acts:Ast_typed.type_expression list) =
  `Typer_typeclass_error (exps,acts)
let wrong_converter (t:Ast_typed.type_expression) = `Typer_converter t
let uncomparable_types (a:Ast_typed.type_expression) (b:Ast_typed.type_expression) =
  `Typer_uncomparable_types (a,b)
let comparator_composed (a:Ast_typed.type_expression) = `Typer_comparator_composed a
let unrecognized_type_constant (e:Ast_core.type_expression) = `Typer_unrecognized_type_constant e

(* new typer errors *)
let constant_declaration_tracer (name: Ast_core.expression_variable) (ae:Ast_core.expression) (expected: Ast_typed.type_expression option) (err:typer_error) =
  `Typer_constant_decl_tracer (name,ae,expected,err)
let in_match_variant_tracer (ae:Ast_core.matching_expr) (err:typer_error) =
  `Typer_match_variant_tracer (ae,err)
let different_types a b = `Typer_different_types (a,b)

let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> typer_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Typer_michelson_comb_no_record loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of type \"michelson_pair\".@.The \"michelson_pair\" type expects a record type as argument. @]"
        Location.pp loc
    | `Typer_michelson_comb_no_variant loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of type \"michelson_or\".@.The \"michelson_or\" type expects a variant type as argument. @]"
        Location.pp loc
    | `Typer_unbound_type_variable (_env,tv,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Type \"%a\" not found. @]"
        Location.pp loc
        Ast_typed.PP.type_variable tv
    | `Typer_unbound_variable (_env,v,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Variable \"%a\" not found. @]"
        Location.pp loc
        Ast_typed.PP.expression_variable v
    | `Typer_match_missing_case (m, v, loc) ->
      let missing = List.fold_left (fun all o -> 
        match List.find_opt (fun f -> f = o) v with 
        | Some _ -> all
        | None -> 
          let (Label o) = o in
          o :: all
      ) [] m in
      let missing = String.concat ", " missing in
      Format.fprintf f
        "@[<hv>%a@.Pattern matching is not exhaustive.@.Cases that are missing: %s. @]"
        Location.pp loc
        missing
    | `Typer_match_extra_case (m, v,loc) ->
      let open Ast_core in
      let rec extra (processed: string list) (redundant: string list) (unknown: string list) = function
      | Label l :: remaining -> (
        match (List.find_opt (fun f -> f = Label l) m)  with
        | Some _ -> (
          match (List.find_opt (fun f -> f = l) processed) with 
          | Some _ -> extra processed (l :: redundant) unknown remaining
          | None -> extra (l :: processed) redundant unknown remaining
        )
        | None -> extra processed redundant (l :: unknown) remaining)
      | [] -> (List.rev redundant, List.rev unknown)
      in
      let (redundant, unknown) = extra [] [] [] v in   
      Format.fprintf f "@[<hv>%a@.Pattern matching over too many cases.@]"
        Location.pp loc;
      if List.length redundant > 0 then (
        let redundant = String.concat ", " redundant in
        Format.fprintf f
          "@[<hv>@.These case(s) are duplicate:@.%s@]"          
          redundant
      );
      if List.length unknown > 0 then (
        let unknown = String.concat ", " unknown in
        Format.fprintf f
          "@[<hv>@.These case(s) don't belong to the variant:@.%s@]"
          unknown
      );
      Format.fprintf f
          "@[<hv>@.Please remove the extra cases. @]"
    | `Typer_unbound_constructor (_env,c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Constructor \"%a\" not found. @]"
        Location.pp loc
        Ast_core.PP.label c
    | `Typer_redundant_constructor (_env,c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid variant.@.Constructor \"%a\" already exists as part of another variant. @]"
        Location.pp loc
        Ast_core.PP.label c    
    | `Typer_type_constant_wrong_number_of_arguments (op,e,a,loc) ->
      Format.fprintf f
        "@[<hv>%a@ Wrong number of arguments for type constant: %a@ expected: %i@ got: %i@]"
        Location.pp loc
        Ast_core.PP.type_constant op
        e a
    | `Typer_michelson_or_no_annotation (c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect usage of type \"michelson_or\".@.The contructor \"%a\" must be annotated with a variant type. @]"
        Location.pp loc
        Ast_core.PP.label c
    | `Typer_program_tracer (_program,err) ->
      Format.fprintf f
        "%a"
        (error_ppformat ~display_format) err
    | `Typer_constant_declaration_tracer (_,_,_,err) ->
      error_ppformat ~display_format f err
    | `Typer_match_error (expected,actual,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Pattern matching over an expression of an incorrect type.@.Type \"%a\" was expected, but got type \"%a\". @]"
        Location.pp loc
        Ast_core.PP.matching_type expected
        Ast_typed.PP.type_expression actual
    | `Typer_needs_annotation (exp,case) ->
      Format.fprintf f
        "@[<hv>%a@.Missing type annotation.@.'%s' needs to be annotated with a type.@]"
        Location.pp exp.location
        case
    | `Typer_fvs_in_create_contract_lambda (e,fvar) ->
      Format.fprintf f
        "@[<hv>%a@.Free variable '%a' is not allowed in CREATE_CONTRACT lambda@]"
        Location.pp e.location
        Ast_typed.PP.expression_variable fvar
    | `Typer_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        Location.pp e.location
    | `Typer_should_be_a_function_type (actual,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type.@.Expected a function type, but got \"%a\". @]"
        Location.pp e.location
        Ast_typed.PP.type_expression actual
    | `Typer_bad_record_access (field,ae,_t,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid record field \"%a\" in record \"%a\". @]"
        Location.pp loc
        Ast_core.PP.label field
        Ast_core.PP.expression ae
    | `Typer_corner_case desc ->
      Format.fprintf f
        "@[<hv>A type system corner case occurred:@.%s@]"
        desc
    | `Typer_bad_collect_loop (t,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Bounded loop over a value with an incorrect type.@.Expected a value with type: \"list\", \"set\" or \"map\", but got a value of type \"%a\". @]"
        Location.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_too_small_record loc ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument provided to Layout.convert_to_(left|right)_comb.@.The record must have at least two elements. @]"
        Location.pp loc
    | `Typer_expression_tracer (e,err) ->
      let exploc = e.location in
      ( match err with 
        | `Typer_assert_equal _ | `Typer_expected_record _
        | `Typer_expected_variant _ | `Typer_wrong_param_number _
        | `Typer_expected_function _ | `Typer_expected_pair _
        | `Typer_expected_list _ | `Typer_expected_set _
        | `Typer_expected_map _ | `Typer_expected_big_map _
        | `Typer_expected_option _ | `Typer_expected_nat _
        | `Typer_expected_bytes _ | `Typer_expected_key _
        | `Typer_expected_signature _ | `Typer_expected_contract _
        | `Typer_expected_string _ | `Typer_expected_key_hash _
        | `Typer_expected_mutez _ | `Typer_expected_op_list _
        | `Typer_expected_int _ | `Typer_expected_bool _
        | `Typer_declaration_order_record _
        | `Typer_not_matching _ | `Typer_uncomparable_types _ | `Typer_typeclass_error _ -> no_loc_children f exploc err
        | _ -> error_ppformat ~display_format f err
      )
    | `Typer_record_access_tracer (_,err) -> error_ppformat ~display_format f err
    | `Typer_not_annotated l ->
      Format.fprintf f "@[<hv>%a@.Can't infer the type of this value, please add a type annotation.@]"
      Location.pp l
    | `Typer_bad_substraction ->
      Format.fprintf f "@[<hv>Invalid subtraction.
The following forms of subtractions are possible:
  * timestamp - int = timestamp
  * timestamp - timestamp = int
  * int/nat - int/nat = int 
  * mutez/tez - mutez/tez = mutez.@]"
    | `Typer_wrong_size t ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect value applied.@.A value with one of the following types is expected: map, list, string, byte or set. @]"
      Location.pp t.location
    | `Typer_wrong_neg t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid value used for negation.@.Expected a value of type nat or int, but got %a. @]"
      Location.pp t.location
      Ast_typed.PP.type_expression t
    | `Typer_wrong_not t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid value used for not operation.@.Expected a value of type Boolean, nat or int, but got %a. @]"
      Location.pp t.location
      Ast_typed.PP.type_expression t
    | `Typer_converter t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of a Michelson converter.@.Converters can only be used on records or variants, but got %a. @]"
      Location.pp t.location
      Ast_typed.PP.type_expression t
    | `Typer_comparator_composed a ->
      Format.fprintf f
        "@[<hv>%a@.Invalid arguments.@.Only composed types of not more than two element are allowed to be compared. @]"
      Location.pp a.location
    | `Typer_constant_decl_tracer (_name,_ae,_expected,err) ->
      Format.fprintf f
        "%a" (error_ppformat ~display_format) err
    | `Typer_match_variant_tracer (_ae,err) ->
      Format.fprintf f
        "%a" (error_ppformat ~display_format) err
    | `Typer_unrecognized_type_constant e ->
      Format.fprintf f
        "@[<hv>%a@.Unrecognized type constant %a. @]"
        Location.pp e.location
        Ast_core.PP.type_expression e
    | `Typer_expected_ascription t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.At this point a block of code is expected, but got \"%a\". @]"
        Location.pp t.location
        Ast_core.PP.expression t
    | `Typer_different_types (a, b) ->
        Format.fprintf f
          "@[<hv>%a@.This expression has type %a, but an expression was expected of type %a.@]"
          Location.pp b.location
          Ast_typed.PP.type_expression b
          Ast_typed.PP.type_expression a      
    |
    ( `Typer_assert_equal _ | `Typer_expected_record _
    | `Typer_expected_variant _ | `Typer_wrong_param_number _
    | `Typer_expected_function _ | `Typer_expected_pair _
    | `Typer_expected_list _ | `Typer_expected_set _
    | `Typer_expected_map _ | `Typer_expected_big_map _
    | `Typer_expected_option _ | `Typer_expected_nat _
    | `Typer_expected_bytes _ | `Typer_expected_key _
    | `Typer_expected_signature _ | `Typer_expected_contract _
    | `Typer_expected_string _ | `Typer_expected_key_hash _
    | `Typer_expected_mutez _ | `Typer_expected_op_list _
    | `Typer_expected_int _ | `Typer_expected_bool _
    | `Typer_declaration_order_record _
    | `Typer_not_matching _ | `Typer_uncomparable_types _ | `Typer_typeclass_error _) as err -> no_loc_children f (Location.Virtual "") err
  )

and no_loc_children : Format.formatter -> Location.t -> typer_error -> unit = fun f exploc err ->
  match err with
  | `Typer_assert_equal (expected,actual) ->
    Format.fprintf f
      "@[<hv>%a@.Invalid type(s).@.Expected: \"%a\", but got: \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression expected 
      Ast_typed.PP.type_expression actual
  | `Typer_expected_record t ->
    Format.fprintf f
      "@[<hv>%a@.Invalid argument.@.Expected a record, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_variant t ->
    Format.fprintf f
      "@[<hv>%a@.Invalid argument.@.Expected a variant, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_wrong_param_number (name,expected,actual) ->
    Format.fprintf f
      "@[<hv>%a@.Function \"%s\" called with wrong number of arguments.@.Expected %d arguments, got %d arguments. @]"
      Location.pp exploc
      name
      expected (List.length actual)
  | `Typer_expected_function e ->
    Format.fprintf f
      "@[<hv>%a@.Invalid argument.@.Expected a function, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_pair e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a tuple, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_list e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a list, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_set e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a set, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_map e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a map, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_big_map e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a big_map, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_option e ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected an option, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression e
  | `Typer_expected_nat t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a nat, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_bytes t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected bytes, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_key t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a key, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_signature t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a signature, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_contract t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a contract, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_string t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a string, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_key_hash t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a key hash, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_mutez t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a mutez, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_op_list t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a list of operations, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_int t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected an int, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_expected_bool t ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument.@.Expected a boolean, but got an argument of type \"%a\". @]"
      Location.pp exploc
      Ast_typed.PP.type_expression t
  | `Typer_not_matching (t1,t2) ->
    Format.fprintf f
      "@[<hv>These types are not matching:@ %a - %a@ %a - %a@]"
      Location.pp t1.location Ast_typed.PP.type_expression t1
      Location.pp t2.location Ast_typed.PP.type_expression t2
  | `Typer_uncomparable_types (a,b) ->
    Format.fprintf f
      "@[<hv>%a@.Invalid arguments.@.These types cannot be compared: \"%a\" and \"%a\". @]"
    Location.pp exploc
    Ast_typed.PP.type_expression a
    Ast_typed.PP.type_expression b
  | `Typer_typeclass_error (exps,acts) ->
    let open Simple_utils.PP_helpers in
    let printl printer ppf args =
      Format.fprintf ppf "(%a)" (list_sep printer (const ", ")) args in
    Format.fprintf f
      "@[<hv>%a@.Invalid arguments.@.Expected an argument of type %a, but got an argument of type %a. @]"
      Location.pp exploc
      (list_sep (printl Ast_typed.PP.type_expression) (const " or ")) exps
      (list_sep Ast_typed.PP.type_expression (const ", ")) acts
  | `Typer_declaration_order_record _loc ->
    Format.fprintf f
      "@[<hv>%a@.Incorrect argument provided to Layout.convert_to_(left|right)_comb.@.The given argument must be annotated with the type of the value. @]"
      Location.pp exploc
  | _ -> ()

let rec error_jsonformat : typer_error -> Yojson.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Typer_michelson_comb_no_record loc ->
    let message = `String "michelson pair comb can only be used on a record type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc)
    ] in
    json_error ~stage ~content
  | `Typer_michelson_comb_no_variant loc ->
    let message = `String "michelson or comb can only be used on a variant type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc)
    ] in
    json_error ~stage ~content
  | `Typer_unbound_type_variable (env,tv,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_typed.PP.type_variable tv in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_unbound_variable (env,v,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_typed.PP.expression_variable v in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_match_missing_case (m, v, loc) ->
    let missing = List.fold_left (fun all o -> 
      match List.find_opt (fun f -> f = o) v with 
      | Some _ -> all
      | None -> 
        let (Label o) = o in
        `String o :: all
    ) [] m in
    let message = `String "Missing match case" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in    
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", `List missing);
    ] in
    json_error ~stage ~content
  | `Typer_match_extra_case (v, m,loc) ->
    let open Ast_core in
    let rec extra processed redundant unknown = function
    | Label l :: remaining -> (
      match (List.find_opt (fun f -> f = Label l) m)  with
      | Some _ -> (
        match (List.find_opt (fun f -> f = l) processed) with 
        | Some _ -> extra processed (`String l :: redundant) unknown remaining
        | None -> extra (l :: processed) redundant unknown remaining
      )
      | None -> extra processed redundant (`String l :: unknown) remaining)
    | [] -> (List.rev redundant, List.rev unknown)
    in
    let (redundant, unknown) = extra [] [] [] v in   
    let message = `String "Redundant case in match cases" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("redundant", `List redundant);
      ("unknown", `List unknown)
    ] in
    json_error ~stage ~content
  | `Typer_unbound_constructor (env,c,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_redundant_constructor (env,c,loc) ->
    let message = `String "redundant constructor" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_type_constant_wrong_number_of_arguments (op, e, a, loc) ->
    let message = `String "Wrong number of arguments for type constant" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let op  = Format.asprintf "%a" Ast_core.PP.type_constant op in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("type_constant", `String op);
      ("expected", `Int e);
      ("actuel", `Int a);
    ] in
    json_error ~stage ~content
  | `Typer_michelson_or_no_annotation (c,loc) ->
    let message = `String "michelson_or must be annotated with a sum type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
    ] in
    json_error ~stage ~content
  | `Typer_program_tracer (p,err) ->
    let message = `String "Typing program" in
    let over = List.fold_left (fun a (p:Ast_core.declaration Location.wrap) -> match p.location with File reg -> Region.cover a reg | Virtual _ -> a) Region.ghost p in
    let loc = `String (Format.asprintf "%a" Location.pp_lift over) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (name,ae,Some t,err) ->
    let message = `String "Typing constant declaration" in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression ae) in
    let loc = `String (Format.asprintf "%a" Location.pp name.location) in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("name", name);
      ("value", value);
      ("expected", expected);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (name,ae,None,err) ->
    let message = `String "Typing constant declaration" in
    let loc = `String (Format.asprintf "%a" Location.pp ae.location) in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("name", name);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_match_error (expected,actual,loc) ->
    let message = `String "Typing match" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let expected = `String (Format.asprintf "%a" Ast_core.PP.matching_type expected) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("actual", actual);
      ("expected", expected);
    ] in
    json_error ~stage ~content
  | `Typer_needs_annotation (exp,case) ->
    let message = `String "This expression needs to be annotated with its type" in
    let loc = `String (Format.asprintf "%a" Location.pp exp.location) in
    let exp = `String (Format.asprintf "%a" Ast_core.PP.expression exp) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", exp);
      ("case", `String case);
    ] in
    json_error ~stage ~content
  | `Typer_fvs_in_create_contract_lambda (e,fvar) ->
    let message = `String "Free variables are not allowed in CREATE_CONTRACT lambdas" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let variable = `String (Format.asprintf "%a" Ast_typed.PP.expression_variable fvar) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("variable", variable);
    ] in
    json_error ~stage ~content
  | `Typer_create_contract_lambda (cst,e) ->
    let message = `String (Format.asprintf "First argument of %a must be inlined using a lambda" Ast_core.PP.constant cst) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
  | `Typer_should_be_a_function_type (actual,e) ->
    let message = `String "expected a function type" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_bad_record_access (field,ae,t,loc) ->
    let message = `String "invalid record field" in
    let field = `String (Format.asprintf "%a" Ast_core.PP.label field) in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression ae) in
    let value_type = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message); ("location", loc);
      ("value", value); ("value_type", value_type);
      ("field", field);
    ] in
    json_error ~stage ~content
  | `Typer_expression_tracer (e,err) ->
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("location", loc);
      ("expression", expression);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_record_access_tracer (e,err) ->
    let message = `String "invalid record access" in
    let expression = `String (Format.asprintf "%a" Ast_typed.PP.expression e) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_assert_equal (expected,actual) ->
    let message = `String "bad types" in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual) in
    let content = `Assoc [
      ("message", message);
      ("expected", expected);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_corner_case desc ->
    let message = `String desc in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_bad_collect_loop (t,loc) ->
    let message = `String "Loops over collections expect lists, sets or maps" in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_declaration_order_record loc ->
    let message = `String "can't retrieve type declaration order in the converted record, you need to annotate it" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_too_small_record loc ->
    let message = `String "converted record must have at least two elements" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_expected_record t ->
    let message = `String "expected a record" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_variant t ->
    let message = `String "expected a record" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_nat t ->
    let message = `String "expected a nat" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_bytes t ->
    let message = `String "expected bytes" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_key t ->
    let message = `String "expected key" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_signature t ->
    let message = `String "expected signature" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_contract t ->
    let message = `String "expected contract" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_string t ->
    let message = `String "expected string" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_key_hash t ->
    let message = `String "expected key hash" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_mutez t ->
    let message = `String "expected mutez" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_op_list t ->
    let message = `String "expected operation lists" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_param_number (name,expected,actual) ->
    let message = `String "constant with a wrong number of parameter" in
    let loc = `String ( Format.asprintf "%a"
      Location.pp_lift
        (List.fold_left (fun a (t:Ast_typed.type_expression) ->
          match t.location with File reg -> Region.cover a reg | Virtual _ -> a) Region.ghost actual)
    ) in
    let value = `String name in
    let expected = `Int expected in
    let actual = `Int (List.length actual) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
      ("actual", actual);
      ("expected", expected);
    ] in
    json_error ~stage ~content
  | `Typer_expected_function e ->
    let message = `String "expected a function" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_pair e ->
    let message = `String "expected a pair" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_list e ->
    let message = `String "expected a list" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_set e ->
    let message = `String "expected a set" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_map e ->
    let message = `String "expected a map" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_big_map e ->
    let message = `String "expected a big map" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_option e ->
    let message = `String "expected an option" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_int e ->
    let message = `String "expected an int" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_bool e ->
    let message = `String "expected a bool" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_not_matching (t1,t2) ->
    let message = `String "types not matching" in
    let loc1 = `String (Format.asprintf "%a" Location.pp t1.location) in
    let loc2 = `String (Format.asprintf "%a" Location.pp t2.location) in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t1) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t2) in
    let content = `Assoc [
      ("message", message);
      ("location_type_1", loc1);
      ("location_type_2", loc2);
      ("type_1", t1);
      ("type_2", t2);
    ] in
    json_error ~stage ~content
  | `Typer_not_annotated _ ->
    let message = `String "not annotated" in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_bad_substraction ->
    let message = `String "bad substraction, bad parameters" in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_size t ->
    let message = `String "should be of type map, list, string, byte or set" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_neg t ->
    let message = `String "should be of type nat or int" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_not t ->
    let message = `String "should be of type bool, nat or int" in
    let loc = `String (Format.asprintf "%a" Location.pp t.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_typeclass_error (exps,acts) ->
    let open Simple_utils.PP_helpers in
    let printl printer ppf args =
      Format.fprintf ppf "%a" (list_sep printer (const " , ")) args in
    let message = `String "typeclass error" in
    let expected = `String (Format.asprintf "%a" (list_sep (printl Ast_typed.PP.type_expression) (const " or ")) exps) in
    let actual = `String (Format.asprintf "%a" (list_sep Ast_typed.PP.type_expression (const " or ")) acts) in
    let content = `Assoc [
      ("message", message);
      ("expected", expected);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_converter t ->
    let message = `String "converters can only be used on records or variants" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in 
    let content = `Assoc [
      ("message", message);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_uncomparable_types (a,b) ->
    let message = `String "those two types are not comparable" in
    let loc1 = `String (Format.asprintf "%a" Location.pp a.location) in
    let loc2 = `String (Format.asprintf "%a" Location.pp b.location) in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression a) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression b) in
    let content = `Assoc [
      ("message", message);
      ("location_type_1", loc1);
      ("location_type_2", loc2);
      ("type_1", t1);
      ("type_2", t2);
    ] in
    json_error ~stage ~content
  | `Typer_comparator_composed a ->
    let message = `String "Only composed types of not more than two element are allowed to be compared" in
    let loc = `String (Format.asprintf "%a" Location.pp a.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_constant_decl_tracer (name,ae,expected,err) ->
    let message = `String "typing constant declaration" in
    let loc = `String (Format.asprintf "%a" Location.pp ae.location) in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let expected = `String (match expected with
      | None -> "(no annotation for the expected type)"
      | Some expected -> Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let content = `Assoc [
      ("message", message) ;
      ("name", name) ;
      ("location", loc) ;
      ("expected", expected) ;
      ("children", error_jsonformat err) ;
    ] in
    json_error ~stage ~content
  | `Typer_match_variant_tracer (m,err) ->
    let message = `String "typing matching expression" in
    let expected = `String (Format.asprintf "%a" Ast_core.PP.matching_type m) in
    let content = `Assoc [
      ("message", message) ;
      ("expected", expected) ;
      ("children", error_jsonformat err) ;
    ] in
    json_error ~stage ~content
  | `Typer_unrecognized_type_constant e ->
    let message = `String "unrecognized type constant" in
    let value = `String (Format.asprintf "%a" Ast_core.PP.type_expression e) in
    let content = `Assoc [
      ("message", message) ;
      ("value", value) ;
    ] in
    json_error ~stage ~content
  | `Typer_expected_ascription t ->
    let message = `String "expected ascription" in
    let location = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression t) in
    let content = `Assoc [
      ("message", message) ;
      ("location", location) ;
      ("value", value) ;
    ] in
    json_error ~stage ~content
  | `Typer_different_types (a,b) ->
    let message = `String ("Types are different.\ 
      Expected these two types to be the same, but they're different") in
    let a = `String (Format.asprintf "%a" Ast_typed.PP.type_expression a) in
    let b = `String (Format.asprintf "%a" Ast_typed.PP.type_expression b) in
    let content = `Assoc [
      ("message", message) ;
      ("a", a) ;
      ("b", b) ;
    ] in
    json_error ~stage ~content
