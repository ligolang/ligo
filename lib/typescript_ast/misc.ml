let make_node_res ?comments node : (string wrap, string) result =
  match node with
  | Ok node ->
    let region = !get_region node in
    let root = Lexeme.read region
    and comments = decode_comments ?comments node in
    Ok (Wrap.make ~comments root region)
  | Error msg -> Error msg

let make_kwd_res ?comments node = make_node_res ?comments node
let make_sym_res ?comments node = make_node_res ?comments node
let dec_identifier_res ?comments node = make_node_res ?comments node
let dec_string_res ?comments node = make_node_res ?comments node

let list_of_children ?(comments = []) decoder children : 'a list =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> []
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    fst_child :: List.fold_right ~f ~init:[] siblings

let ne_list_opt_of_children ?(comments = []) decoder children : 'a ne_list option =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match children with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    Some Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings)

let ne_list_of_children ?(comments = []) decoder children : ('a ne_list, _) result =
  match ne_list_opt_of_children ~comments decoder children with
  | None -> Error "Expected at least one child."
  | Some ne_list -> Ok ne_list

let wrap_children ?(comments = []) decoder node : 'a ne_list wrap option =
  let f raw_child = List.cons (decoder ?comments:None raw_child) in
  match collect_named_children node with
  | [] -> None
  | fst_raw_child :: siblings ->
    let fst_child = decoder ?comments:(Some comments) fst_raw_child in
    let stmts = Nonempty_list.(fst_child :: List.fold_right ~f ~init:[] siblings) in
    let region = !get_region node in
    Some (Wrap.make stmts region)

let decode_enclosed ?(comments = []) node decoder opening closing : 'a enclosed =
  ensure_Ok node
  @@
  let comments = comments @ prev_comments node in
  let* opening = first_child_named opening node in
  let opening = make_sym ~comments opening in
  let* closing = first_child_named closing node in
  let closing = make_sym closing in
  let* child = (* We assume one child *) child_ranked 1 node in
  let contents = decoder child in
  Ok { opening; contents; closing }

let decode_braces ?comments node decoder : 'a braces =
  Braces (decode_enclosed ?comments node decoder "{" "}")

let decode_chevrons ?comments node decoder : 'a chevrons =
  Chevrons (decode_enclosed ?comments node decoder "<" ">")

let decode_brackets ?comments node decoder : 'a brackets =
  Brackets (decode_enclosed ?comments node decoder "[" "]")

let decode_parens ?comments node decoder : 'a parens =
  Parens (decode_enclosed ?comments node decoder "(" ")")
