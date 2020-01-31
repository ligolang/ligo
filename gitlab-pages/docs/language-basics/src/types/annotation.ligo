type int_map is map(int, int);
function get_first(const int_map: int_map): option(int) is int_map[1]
// empty map needs a type annotation
const first: option(int) = get_first(((map end) : int_map ));