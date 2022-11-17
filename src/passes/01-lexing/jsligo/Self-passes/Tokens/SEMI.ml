(* Insertion of semicolons à la JavaScript *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

(* Insertion *)

let semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (SEMI _ as semi) :: (LineCom _ as t) :: rest
  | (SEMI _ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Directive _ as t)  :: rest
  | (SEMI _ as semi) :: (Namespace _ as t)  :: rest
  | (SEMI _ as semi) :: (Export _ as t)  :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (LineCom _ as t)  :: rest
  | (LBRACE _ as semi) :: (BlockCom _ as t)  :: rest
  | (LBRACE _ as semi) :: (Directive _ as t)  :: rest
  | (LBRACE _ as semi) :: (Namespace _ as t)  :: rest
  | (LBRACE _ as semi) :: (Export _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest
  | (COLON _ as semi) :: (LineCom _ as t)  :: rest
  | (COLON _ as semi) :: (BlockCom _ as t)  :: rest
  | (COLON _ as semi) :: (Directive _ as t)  :: rest
  | (COLON _ as semi) :: (Namespace _ as t)  :: rest
  | (COLON _ as semi) :: (Export _ as t)  :: rest
  | (COLON _ as semi) :: (Let _ as t)  :: rest
  | (COLON _ as semi) :: (Const _ as t)  :: rest
  | (COLON _ as semi) :: (Type _ as t)  :: rest
  | (COLON _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | (SEMI _) :: (Case _ as t) :: rest
  | (SEMI _) :: (Default _ as t) :: rest
  | (SEMI _) :: (Else _ as t) :: rest ->
    inner (t :: result) rest
  | (RBRACE _ as rbrace) :: (LineCom _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (BlockCom _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Directive _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Namespace _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Export _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Let _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Const _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Type _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Return _ as r)  :: rest ->
    inner (r :: Token.ghost_SEMI :: rbrace :: result ) rest
  | token :: (LineCom _ as t) :: rest
  | token :: (BlockCom _ as t) :: rest
  | token :: (Directive _ as t) :: rest
  | token :: (Namespace _ as t) :: rest
  | token :: (Export _ as t) :: rest
  | token :: (Let _ as t) :: rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI (Token.wrap ";" (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest
    )
    else (
      match token with
        RBRACE _ as t ->
        inner (t :: SEMI (Token.wrap_semi (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop)) :: token :: result) rest
      | _ ->
        inner (t :: token :: result) rest
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  Ok (inner [] tokens)

(* Exported *)

let filter
    : ?print_passes:Std.t ->
      add_warning:(Main_warnings.all -> unit) ->
      Token.t list ->
      _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Automatic semicolon insertion.")
    | None -> ()
  in semicolon_insertion tokens
