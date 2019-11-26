//Test deep_access in PascalLigo
type pii is (int*int)
type ppi is record x:pii; y:pii end
type ppp is (ppi*ppi)

function main (const toto : unit) : int is block {
  var a : ppp := (
    record
      x = (0,1);
      y = (10,11);
    end ,
    record
      x = (100,101);
      y = (110,111);
    end
  ) ;
  a.0.x.0 := 2;
  const b:int = a.0.x.0;
} with b


function asymetric_tuple_access(const foo : unit) : int is block {
  var mytuple : int * (int * (int * int)) := (0,(1,(2,3))) ;
} with mytuple.0 + mytuple.1.0 + mytuple.1.1.0 + mytuple.1.1.1

type nested_record_t is record
  nesty : (record mymap : map(int,string) ; end) ;
end
function nested_record (var nee : nested_record_t) : string is block {
    nee.nesty.mymap[1] := "one" ;
} with ( get_force(1, nee.nesty.mymap) )