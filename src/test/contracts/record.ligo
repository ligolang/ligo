// Test record type in PascaLIGO

type foobar is record
     foo : int ;
     bar : int ;
end

const fb : foobar = record
  foo = 0 ;
  bar = 0 ;
end

type abc is record
  a : int ;
  b : int ;
  c : int ;
end

const abc : abc = record
  a = 42 ;
  b = 142 ;
  c = 242 ;
end

const a : int = abc.a ;
const b : int = abc.b ;
const c : int = abc.c ;

function projection (const r : foobar) : int is
  begin
    skip
  end with r.foo + r.bar

function modify (const r : foobar) : foobar is
  block {
    r.foo := 256 ;
  } with r

function modify_abc (const r : abc) : abc is
  block {
    r := r with record b = 2048; end;
  } with r

type big_record is record
     a : int ;
     b : int ;
     c : int ;
     d : int ;
     e : int ;
end

const br : big_record = record
  a = 23 ;
  b = 23 ;
  c = 23 ;
  d = 23 ;
  e = 23 ;
end
