function local_type (var u : unit) : int is {
  const y = 42;
  type toto is int;
  function foo (const b : toto) : toto is b
} with titi

const x = 42