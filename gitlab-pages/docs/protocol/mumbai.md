---
id: mumbai
title: Mumbai
description: Mumbai changes
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


## API

### Deprecation

The type `tx_rollup_l2_address` has been disabled (see the [changelog](https://tezos.gitlab.io/protocols/016_mumbai.html#breaking-changes) for the Mumbai protocol).

### New operators

#### Bitwise operators of `bytes`

Bitwise operations are now supported on `bytes`

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=test_bitwise
const test_foo = {
  const b_and           = Bitwise.and         (0x0005, 0x0106);
  const b_or            = Bitwise.or          (0x0005, 0x0106);
  const b_xor           = Bitwise.xor         (0x0005, 0x0106);
  const b_shift_left    = Bitwise.shift_left  (0x06  , 8n    );
  const b_shift_right   = Bitwise.shift_right (0x0006, 1n    );
} with assert (b_and         = 0x0004 and
               b_or          = 0x0107 and
               b_xor         = 0x0103 and
               b_shift_left  = 0x0600 and
               b_shift_right = 0x0003   )
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=test_bitwise
let test_bytes_bitwise_ops  =
  let b_and         = 0x0005 land 0x0106 in
  let b_or          = 0x0005 lor  0x0106 in
  let b_xor         = 0x0005 lxor 0x0106 in
  let b_shift_left  = 0x06   lsl  8n     in
  let b_shift_right = 0x0006 lsr  1n     in

  assert (b_and         = 0x0004 &&
          b_or          = 0x0107 &&
          b_xor         = 0x0103 &&
          b_shift_left  = 0x0600 &&
          b_shift_right = 0x0003  )
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_bitwise
const test_bytes_bitwise_module = (() => {
  const b_and           = Bitwise.and         (0x0005, 0x0106  );
  const b_or            = Bitwise.or          (0x0005, 0x0106  );
  const b_xor           = Bitwise.xor         (0x0005, 0x0106  );
  const b_shift_left    = Bitwise.shift_left  (0x06  , 8 as nat);
  const b_shift_right   = Bitwise.shift_right (0x0006, 1 as nat);

  assert (b_and         == 0x0004 &&
          b_or          == 0x0107 &&
          b_xor         == 0x0103 &&
          b_shift_left  == 0x0600 &&
          b_shift_right == 0x0003  )})()
```

</Syntax>

#### Conversion between `bytes`-`int` & `bytes`-`nat`

We can now convert between `bytes`-`int` & `bytes`-`nat` using the functions
- `int`   (Convert `bytes` to `int`)
- `nat`   (Convert `bytes` to `nat`)
- `bytes` (Convert `bytes` to either `int` or `nat`)

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=test_bytes_conv
(* bytes -> nat *)
const test_bytes_nat = nat(0x1234) // 1234n

(* nat -> bytes *)
const test_nat_bytes = bytes(4660n) // 0x1234

(* bytes -> int *)
const test_bytes_int = int(0x1234) // 4660

(* int -> bytes *)
const test_int_bytes = bytes(4660) // 0x1234
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=test_bytes_conv
(* bytes -> nat *)
let test_bytes_nat = nat 0x1234 (* 1234n *)

(* nat -> bytes *)
let test_nat_bytes = bytes 4660n (* 0x1234 *)

(* bytes -> int *)
let test_bytes_int = int 0x1234 (* 4660 *)

(* int -> bytes *)
let test_int_bytes = bytes 4660 (* 0x1234 *)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=test_bytes_conv
/* bytes -> nat */
const test_bytes_nat = nat(0x1234) // (1234 as nat)

/* nat -> bytes */
const test_nat_bytes = bytes(4660 as nat) // 0x1234

/* bytes -> int */
const test_bytes_int = int(0x1234) // 4660

/* int -> bytes */
const test_int_bytes = bytes(4660) // 0x1234
```

</Syntax>
