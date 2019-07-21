function iter_op (const s : set(int)) : int is
  var r : int := 0 ;
  function aggregate (const i : int) : unit is
  begin
    r := r + i ;
  end with unit
  begin
    set_iter(s , aggregate) ;
  end with r

const s_e : set(string) = (set_empty : set(string))

const s_fb : set(string) = set [
      "foo" ;
      "bar" ;
]

function add_op (const s : set(string)) : set(string) is
  begin skip end with set_add("foobar" , s)

function remove_op (const s : set(string)) : set(string) is
  begin skip end with set_remove("foobar" , s)

function mem_op (const s : set(string)) : bool is
  begin skip end with set_mem("foobar" , s)

