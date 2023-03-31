(* Insertion of semicolons à la JavaScript *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

(* Insertion *)

let mk_semi r =
  let open! Token in
  let start = (r#shift_one_uchar (-1))#stop in
  let stop = r#stop in
  let region = Region.make ~start ~stop in
  SEMI (Token.wrap_semi region)

let rec there_is_a_decl_next tokens =
  let open! Token in
  match tokens with
  | [] -> false
  | LineCom _ :: tokens
  | BlockCom _ :: tokens ->
    there_is_a_decl_next tokens
  | Directive _ :: _
  | Namespace _ :: _
  | Export _ :: _
  | Let _ :: _
  | Const _ :: _
  | Type _ :: _-> true
  | _ -> false

let rec there_is_else_case_default tokens =
  let open! Token in
  match tokens with
  | [] -> false
  | LineCom _  :: tokens
  | BlockCom _ :: tokens ->
    there_is_else_case_default tokens
  | Else _    :: _
  | Case _    :: _
  | Default _ :: _ -> true
  | _ -> false

let semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (Export _ as export) :: rest ->
    inner (export :: result) rest
  | (RBRACE _ as rbrace) :: (LineCom _ as t)  :: rest
  | (RBRACE _ as rbrace) :: (BlockCom _ as t) :: rest ->
    if there_is_else_case_default rest then
      inner (t :: rbrace :: result) rest
    else
      let (s, _) = Token.proj_token rbrace in
      inner (t:: mk_semi s :: rbrace :: result) rest
  | (SEMI _ as semi) :: (LineCom _ as t) :: rest
  | (SEMI _ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Directive _ as t)  :: rest
  | (SEMI _ as semi) :: (Namespace _ as t)  :: rest
  | (SEMI _ as semi) :: (Export _ as t)  :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (LineCom _ as t) :: rest
  | (LBRACE _ as semi) :: (BlockCom _ as t) :: rest
  | (LBRACE _ as semi) :: (Directive _ as t)  :: rest
  | (LBRACE _ as semi) :: (Namespace _ as t)  :: rest
  | (LBRACE _ as semi) :: (Export _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest
  | (COLON _ as semi) :: (LineCom _ as t) :: rest
  | (COLON _ as semi) :: (BlockCom _ as t) :: rest
  | (COLON _ as semi) :: (Directive _ as t)  :: rest  
  | (COLON _ as semi) :: (Namespace _ as t)  :: rest
  | (COLON _ as semi) :: (Export _ as t)  :: rest
  | (COLON _ as semi) :: (Let _ as t)  :: rest
  | (COLON _ as semi) :: (Const _ as t)  :: rest
  | (COLON _ as semi) :: (Type _ as t)  :: rest
  | (COLON _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | token :: (LineCom _ as t) :: rest
  | token :: (BlockCom _ as t) :: rest ->
    if there_is_a_decl_next rest then
      let (r, _) = Token.proj_token token in
      inner (t :: mk_semi r :: token :: result) rest
    else
      inner (t :: token :: result) rest
  | (SEMI _) :: (Case _ as t) :: rest
  | (SEMI _) :: (Default _ as t) :: rest
  | (SEMI _) :: (Else _ as t) :: rest ->
    inner (t :: result) rest
  | (RBRACE _ as rbrace) :: (Directive _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Namespace _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Export _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Let _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Const _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Ident _ as r)  :: rest
  | (RBRACE _ as rbrace) :: (Type _ as r)  :: rest ->
    let (s, _) = Token.proj_token rbrace in
    inner (r :: mk_semi s :: rbrace :: result ) rest
  | (RPAR _ as hd) :: tl
  | (Else _ as hd) :: tl when not (there_is_a_decl_next tl) -> 
    inner (hd :: result) tl
  | token :: (Directive _ as t) :: rest
  | token :: (Namespace _ as t) :: rest
  | token :: (Export _ as t) :: rest
  | token :: (Let _ as t) :: rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    let semi = mk_semi r in
    if r#stop#line < r2#start#line  then (
      inner (t :: semi :: token :: result) rest
    )
    else (
      match token with
        RBRACE _ as t ->
        inner (t :: semi :: token :: result) rest
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
