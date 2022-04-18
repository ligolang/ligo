module Foo is {
  type foo (_a) is _a * _a
}

type fii is Foo.foo (int)