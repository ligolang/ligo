open Simple_utils.Trace

let fold_map__list continue state v =
  let aux = fun acc elt ->
    let%bind (state , lst') = acc in
    let%bind (state , elt') = continue state elt in
    ok (state , elt' :: lst') in
  List.fold_left aux (ok (state, [])) v


let fold_map__option continue state v =
  match v with
    Some x -> continue state x
  | None -> ok None
