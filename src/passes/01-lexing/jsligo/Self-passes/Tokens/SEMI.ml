(* Insertion of semicolons à la JavaScript *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

(* Insertion *)

let semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    Directive _ as t :: rest ->
      inner (t :: result) rest
  | LineCom _ as t :: rest ->
      inner (t :: result) rest
  | BlockCom _ as t :: rest ->
      inner (t :: result) rest
  | semi               :: (LineCom _ as t)    :: rest
  | semi               :: (BlockCom _ as t)   :: rest
  | (SEMI _ as semi)   :: (Namespace _ as t)  :: rest
  | (SEMI _ as semi)   :: (Export _ as t)     :: rest
  | (SEMI _ as semi)   :: (Let _ as t)        :: rest
  | (SEMI _ as semi)   :: (Const _ as t)      :: rest
  | (SEMI _ as semi)   :: (Type _ as t)       :: rest
  | (SEMI _ as semi)   :: (Return _ as t)     :: rest
  | (LBRACE _ as semi) :: (Namespace _ as t)  :: rest
  | (LBRACE _ as semi) :: (Export _ as t)     :: rest
  | (LBRACE _ as semi) :: (Let _ as t)        :: rest
  | (LBRACE _ as semi) :: (Const _ as t)      :: rest
  | (LBRACE _ as semi) :: (Type _ as t)       :: rest
  | (LBRACE _ as semi) :: (Return _ as t)     :: rest ->
      inner (t :: semi :: result) rest
  | token :: (Namespace _ as t) :: rest
  | token :: (Export _ as t)    :: rest
  | token :: (Let _ as t)       :: rest
  | token :: (Const _ as t)     :: rest
  | token :: (Type _ as t)      :: rest
  | token :: (Return _ as t)    :: rest ->
      let r,  _ = Token.proj_token token
      and r2, _ = Token.proj_token t in
      if r#stop#line < r2#start#line  then
        let start  = (r#shift_one_uchar (-1))#stop
        and stop   = r#stop in
        let region = Region.make ~start ~stop in
        let semi   = Token.mk_SEMI region in
        inner (t :: semi :: token :: result) rest
      else (
        match token with
          RBRACE _ ->
            let start  = (r#shift_one_uchar (-1))#stop
            and stop   = r#stop in
            let region = Region.make ~start ~stop in
            let semi   = Token.mk_SEMI region in
            inner (token :: semi :: token :: result) rest
        | _ ->
           inner (t :: token :: result) rest)
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
