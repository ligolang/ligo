module Bytes_ = {
  let pack_  = (type a, x : a) => Bytes.pack(x)
  let unpack_  = (type a, x : bytes) => (Bytes.unpack(x) : option (a))
}

module Foo = Bytes_

let packer = (type b, x : b) => Foo.pack_(x)

let foo = (packer(1), packer("hello"))

let bar =
  let (x, y) = foo;
  ((Bytes_.unpack_(x) : option(int)), (Bytes_.unpack_(y) : option(string)))
