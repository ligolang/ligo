function inc ( const i : int ) : int is
 block { skip } with i + 1

function double_inc ( const i : int ) : int is
 block { skip } with inc(inc(i))

function foo ( const i : int ) : int is
 block { skip } with inc(i) + double_inc(i)
