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
