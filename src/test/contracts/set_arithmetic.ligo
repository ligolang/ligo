// Test set type and basic operations in PascaLIGO

const s_e : set(string) = (set_empty : set(string))

const s_fb : set(string) = set [
      "foo" ;
      "bar" ;
]

function add_op (const s : set(string)) : set(string) is
  begin skip end with set_add("foobar" , s)

function remove_op (const s : set(string)) : set(string) is
  begin skip end with set_remove("foobar" , s)

// Test the PascaLIGO syntactic sugar for set removal vs. the function call
function remove_syntax (var s : set(string)) : set(string) is
  begin remove "foobar" from set s; end with s

function remove_deep (var s : set(string) * nat) : set(string) * nat is
  begin remove "foobar" from set s.0; end with s

function patch_op (var s: set(string)) : set(string) is
  begin patch s with set ["foobar"]; end with s

function patch_op_deep (var s: set(string)*nat) : set(string)*nat is
  begin patch s.0 with set ["foobar"]; end with s

function patch_op_empty (var s: set(string)) : set(string) is
  begin patch s with set []; end with s

function mem_op (const s : set(string)) : bool is
  begin skip end with set_mem("foobar" , s)



