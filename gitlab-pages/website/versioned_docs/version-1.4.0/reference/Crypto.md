---
id: crypto-reference
title: Crypto
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


Cryptographic primitives


<SyntaxTitle syntax="cameligo">
val blake2b : bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let blake2b: (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
Compute the cryptographic hash of the top of the stack using the
    Blake2b-256 cryptographic hash function.


<SyntaxTitle syntax="cameligo">
val sha256 : bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha256: (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
Compute the cryptographic hash of the top of the stack using the
    SHA-256 cryptographic hash function.


<SyntaxTitle syntax="cameligo">
val sha512 : bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha512: (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
Compute the cryptographic hash of the top of the stack using the
    SHA-512 cryptographic hash function.


<SyntaxTitle syntax="cameligo">
val sha3 : bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let sha3: (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
Compute the cryptographic hash of the top of the stack using the
    SHA3-256 cryptographic hash function.


<SyntaxTitle syntax="cameligo">
val keccak : bytes -&gt; bytes
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let keccak: (&#95;: bytes) =&gt; bytes
</SyntaxTitle>
Compute the cryptographic hash of the top of the stack using the
    Keccak-256 cryptographic hash function.


<SyntaxTitle syntax="cameligo">
val hash&#95;key : key -&gt; key&#95;hash
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let hash&#95;key: (&#95;: key) =&gt; key&#95;hash
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `hash_key k` computes the Base58Check of the public key
    `k`.

</Syntax>

<Syntax syntax="jsligo">

The call `hash_key(k)` computes the Base58Check of the public key
    `k`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val check : key -&gt; signature -&gt; bytes -&gt; bool
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let check: (&#95;: key) =&gt; (&#95;: signature) =&gt; (&#95;: bytes) =&gt; bool
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `check k s b` verifies that the byte sequence `b` has
    been signed with the key `k`: it is `true` if, and only if, the
    signature `s` is a valid signature of the byte sequence created
    with `k`.

</Syntax>

<Syntax syntax="jsligo">

The call `check(k, s, b)` verifies that the byte sequence `b` has
    been signed with the key `k`: it is `true` if, and only if, the
    signature `s` is a valid signature of the byte sequence created
    with `k`.

</Syntax>
