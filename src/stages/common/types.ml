type expression_
and expression_variable = expression_ Var.t Location.wrap
let expression_variable_to_yojson var = Location.wrap_to_yojson (Var.to_yojson) var
let expression_variable_of_yojson var = Location.wrap_of_yojson (Var.of_yojson) var
type type_
and type_variable = type_ Var.t
let type_variable_to_yojson var = Var.to_yojson var
let type_variable_of_yojson var = Var.of_yojson var

type label = Label of string
let label_to_yojson (Label l) = `List [`String "Label"; `String l]
let label_of_yojson = function
  | `List [`String "Label"; `String l] -> Ok (Label l)
  | _ -> Utils.error_yojson_format "Label of string"


module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)
type 'a label_map = 'a LMap.t

and ('a,'b) binder = ('a * 'b)


include Enums
include Enums_utils

let const_name = function
  | Deprecated {const;_} -> const
  | Const      const     -> const
let bindings_to_yojson f g xs = `List (List.map (fun (x,y) -> `List [f x; g y]) xs)
let label_map_to_yojson row_elem_to_yojson m =
  bindings_to_yojson label_to_yojson row_elem_to_yojson (LMap.bindings m)

let binding_of_json f g = function
  | `List [x;y] ->
     begin match f x, g y with
     | Ok x, Ok y -> Some (x,y)
     | _ -> None end
  | _ -> None

let err_bad_format =
  Utils.error_yojson_format
    "A label map, represented as an array [ [string , element] , ... ]."

let bindings_of_yojson f g = function
  | `List xs ->
     begin match Option.bind_map_list (binding_of_json f g) xs with
     | None -> err_bad_format
     | Some xs -> Ok xs end
  | _ -> err_bad_format

let label_map_of_yojson row_elem_of_yojson m =
  Stdlib.Result.map LMap.of_list (bindings_of_yojson label_of_yojson row_elem_of_yojson m)

let binder_to_yojson f g (a,b) = `List [f a; g b]
let binder_of_yojson f g json = match json with
  | `List [a;b] -> (
    match f a with 
      Ok a -> 
      ( match g b with
          Ok b -> Ok (a,b)
        | Error e -> Error e)
    | Error e -> Error e
    )
  | _ -> err_bad_format
