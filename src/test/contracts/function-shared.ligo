function inc ( const i : int ) : int is
 block { skip } with i + 1

function double_inc ( const i : int ) : int is
 block { skip } with inc(i + 1)

function foo ( const i : int ) : int is
 block { skip } with inc(i) + double_inc(i)
