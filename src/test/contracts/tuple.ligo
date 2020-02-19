type abc is (int * int * int)

function projection_abc (const tpl : abc) : int is
  block { skip } with tpl.2

function modify_abc (const tpl : abc) : abc is
  block {
    tpl.2 := 2048 ;
  } with tpl

type foobar is (int * int)

const fb : foobar = (0, 0)

function projection (const tpl : foobar) : int is
  begin
    skip
  end with tpl.1 + tpl.2

type big_tuple is (int * int * int * int * int)

const br : big_tuple = (23, 23, 23, 23, 23)
