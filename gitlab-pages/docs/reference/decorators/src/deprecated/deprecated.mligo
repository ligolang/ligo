[@inline] [@deprecated "Use `List.tail` instead."]
let tail_opt (type elt) (list: elt List.t) : elt List.t option =
  List.tail list