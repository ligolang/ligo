type t is int * string
type u is t

type v is record
            foo: key;
            bar: mutez;
            baz: address
          end

type w is K of (U of int) // v * u

type i is int;

const x : v =
   record
     foo = 4;
     bar = 5;
     baz = 0x3244
   end

(* Block comment *)

entrypoint g (storage s : u; const l : list (int))
  : operation (list) is
  var m : map (int, string) := empty_map;
  var y : v := copy x with record bar = 7 end;

  function f (const x : int) : int is
     var y : int := 5 - x
     const z : int = 6
     begin
       y := x + y
     end with y * 2

  begin
     y.[4] := "hello";
     match l with
        [] -> null
     | h#t -> q (h+2)
     end;
     begin
       g (Unit);
       fail "in extremis"
     end
  end with (s, ([]: (u * operation (list))))
