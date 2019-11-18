// Test different ways of calling functions in PascaLIGO

type foo is record
  bar : int -> int ;
end

function f (const i : int) : int is
  begin
    skip
  end with i

function g (const i : unit) : int -> int is
  begin skip end with f

const r : foo = record
  bar = f ;
end

const x : int = f(42)
const y : int = r.bar(42)
const z : int = (g(unit))(42)
