[@@@warning "-42"]

module Core   = LexerLib.Core
module Region = Simple_utils.Region

let ok x = Stdlib.Ok x

let set_markup markup tokens =
  let open! Token in
  let set_markup token markup =
    let token = Region.set_markup token markup in
    token
  in
  match tokens with
    token :: rest -> (
      (match token with
        ARROW token -> ARROW (set_markup token markup)
      | CONS token  -> CONS (set_markup token markup)
      | CARET token -> CARET (set_markup token markup)
      | MINUS token -> MINUS (set_markup token markup)
      | PLUS token  -> PLUS (set_markup token markup)
      | SLASH token -> SLASH (set_markup token markup)
      | TIMES token -> TIMES (set_markup token markup)
      | LPAR token -> LPAR (set_markup token markup)
      | RPAR token -> RPAR (set_markup token markup)
      | LBRACKET token -> LBRACKET (set_markup token markup)
      | RBRACKET token -> RBRACKET (set_markup token markup)
      | LBRACE token -> LBRACE (set_markup token markup)
      | RBRACE token -> RBRACE (set_markup token markup)
      | COMMA token -> COMMA (set_markup token markup)
      | SEMI token -> SEMI (set_markup token markup)
      | VBAR token -> VBAR (set_markup token markup)
      | COLON token -> COLON (set_markup token markup)
      | DOT token -> DOT (set_markup token markup)
      | WILD token -> WILD (set_markup token markup)
      | EQ token -> EQ (set_markup token markup)
      | NE token -> NE (set_markup token markup)
      | LT token -> LT (set_markup token markup)
      | GT token -> GT (set_markup token markup)
      | LE token -> LE (set_markup token markup)
      | GE token -> GE (set_markup token markup)
      | BOOL_OR token -> BOOL_OR (set_markup token markup)
      | BOOL_AND token -> BOOL_AND (set_markup token markup)
      | QUOTE token -> QUOTE (set_markup token markup)
      | Begin token -> Begin (set_markup token markup)
      | Else token -> Else (set_markup token markup)
      | End token -> End (set_markup token markup)
      | Fun token -> Fun (set_markup token markup)
      | Rec token -> Rec (set_markup token markup)
      | If token -> If (set_markup token markup)
      | In token -> In (set_markup token markup)
      | Let token -> Let (set_markup token markup)
      | Match token -> Match (set_markup token markup)
      | Mod token -> Mod (set_markup token markup)
      | Land token -> Land (set_markup token markup)
      | Lor token -> Lor (set_markup token markup)
      | Lxor token -> Lxor (set_markup token markup)
      | Lsl token -> Lsl (set_markup token markup)
      | Lsr token -> Lsr (set_markup token markup)
      | Not token -> Not (set_markup token markup)
      | Of token -> Of (set_markup token markup)
      | Or token -> Or (set_markup token markup)
      | Then token -> Then (set_markup token markup)
      | Type token -> Type (set_markup token markup)
      | With token -> With (set_markup token markup)
      | Module token -> Module (set_markup token markup)
      | Struct token -> Struct (set_markup token markup)
      | EOF token -> EOF (set_markup token markup)
      | String i -> String {i with region = set_markup i.region markup}
      | Verbatim i -> Verbatim {i with region = set_markup i.region markup}
      | Bytes i -> Bytes {i with region = set_markup i.region markup}
      | Int i -> Int {i with region = set_markup i.region markup}
      | Nat i -> Nat {i with region = set_markup i.region markup}
      | Mutez i -> Mutez {i with region = set_markup i.region markup}
      | Ident i -> Ident {i with region = set_markup i.region markup}
      | UIdent i -> UIdent {i with region = set_markup i.region markup}
      | Lang i -> Lang {i with region = set_markup i.region markup}
      | Attr i -> Attr {i with region = set_markup i.region markup}
      | Directive (Linemarker d) ->
         Directive (Linemarker {d with region = set_markup d.region markup})
    ) :: rest)
  | [] -> (
    let region = match markup with
      Region.LineCom ({region; _}, _) :: _ -> region
    | BlockCom ({region; _}, _) :: _ -> region
    | [] -> failwith "should not happen"
    in
    [EOF (set_markup region markup)]
  )

let rec lex_unit_to_closest_token_region = function
  Core.Token t :: _ -> Token.to_region t
| Markup (Tabs {region; _} | Space {region; _} | Newline {region;_}
         | LineCom {region;_} | BlockCom {region; _} | BOM {region; _} ) :: [] -> region
| Markup _ :: rest -> lex_unit_to_closest_token_region rest
| Directive (Linemarker {region;_}) :: [] -> region
| Directive _ :: rest -> lex_unit_to_closest_token_region rest
| [] -> Region.ghost

let rec lex_unit_to_closest_token_region2 = function
  Core.Token t :: _ -> Token.to_region t
| Markup (LineCom {region;_} | BlockCom {region; _}) :: _ -> region
| Markup _ :: rest -> lex_unit_to_closest_token_region2 rest
| Directive (Linemarker {region;_}) :: _ -> region
| [] -> Region.ghost

let attach = function
  Stdlib.Ok lex_units ->
    let rec apply_comment create_comment (region: Region.t) (value: string) result rest markup_queue =
      (match result, rest with
        _, [] ->
          apply (set_markup (create_comment (({region; value}: _ Region.reg), Region.Before) :: markup_queue) result) rest []
      | [], Core.Token hd :: rest ->
        apply (hd :: result) rest []
      | [], _ -> apply (set_markup (create_comment ({region; value}, Region.After) :: markup_queue) result) rest []
      | next_token :: _, (prev_token :: prev_rest) ->
        let token = Token.to_region next_token in
        let comment_stop_line = region#stop#line in
        let next_line = match token#markup with
          [] -> token#start#line
        | Region.BlockCom ({region; _},_) :: _
        | Region.LineCom ({region; _},_) :: _ -> region#start#line
        in
        let previous_line = (lex_unit_to_closest_token_region2 rest)#stop#line in

        if comment_stop_line + 1 = next_line && previous_line < comment_stop_line then
          (* before next token *)

          apply (set_markup (create_comment ({region; value}, Region.Before) :: token#markup) result) rest []
        else  (
          let r = lex_unit_to_closest_token_region rest in
          let pos = if r#start#line = region#stop#line then
            Region.Inline (* inline after previous token *)
          else
            After (* after previous token *)
          in
          match prev_token with
            Core.Token token ->
              apply (set_markup (create_comment ({region; value}, pos) :: markup_queue) (token :: result)) prev_rest []
          | _ ->
            apply result prev_rest ((create_comment ({region; value}, pos)) :: markup_queue)
        )
      )
    and apply result tokens markup_queue =
      match tokens with
        Core.Token token :: rest ->
          apply (set_markup markup_queue (token::result)) rest []
      | Core.Markup (BlockCom c) :: rest ->
        let value = String.sub c.value 2 (String.length c.value - 4) in
        apply_comment (fun (a, b) -> Region.BlockCom (a, b)) c.region value result rest markup_queue
      | Core.Markup (LineCom c) :: rest ->
        let value = String.sub c.value 2 (String.length c.value - 2) in
        apply_comment (fun (a, b) -> Region.LineCom (a, b)) c.region value result rest markup_queue
      | Core.Markup _  :: rest -> apply result rest markup_queue
      | Core.Directive d  :: rest -> apply (Token.Directive d :: result) rest markup_queue
      | [] -> result
    in
    ok @@ apply [] (List.rev lex_units) []
  | Error _ as err -> err
