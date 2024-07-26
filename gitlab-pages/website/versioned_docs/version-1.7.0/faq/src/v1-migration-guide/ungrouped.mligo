[@entry]
let set_storage (new_storage : int)  (_old_storage : int): operation list * int = ([], new_storage)

[@view]
let get_storage () (storage : int): int = storage
let stuff = {
    x = "foo";
    y = (42, "life", { universe = true });
}

let part : bool = stuff.y.2.universe