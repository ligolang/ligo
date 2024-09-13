// type t = <T>(x: T) => T;

type t = abstract new <T>(@foo @bar("baz") public override readonly this:T=v, y?:T) => T;

// type t = infer u extends {};
// type a = [...[{y=4}, x=5] : c];
//type a = [...{...f, x, k:v, y=3} : c];
//{
//type t<T extends U = A> = [x: T, y?: number, string?, ...string, any[]];
//{}
//}
// type t = readonly number;
// f <string | (number)>("", x);

//[...x, 6];
