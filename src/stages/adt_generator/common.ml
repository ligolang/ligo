type ('a,'err) monad = ('a,'err) Simple_utils.Trace.result;;
let (>>?) v f = Simple_utils.Trace.bind f v;;
let return v = Simple_utils.Trace.ok v;;
