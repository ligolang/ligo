type foobar is (int * int)

const fb : foobar = (0, 0)

function projection (const tpl : foobar) : int is
  begin
    skip
  end with tpl.0 + tpl.1

type big_tuple is (int * int * int * int * int)

const br : big_tuple = (23, 23, 23, 23, 23)
