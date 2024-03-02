---
id: test.next.typed-address-reference
title: Typed_address
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val transfer : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p -&gt; tez -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; test&#95;exec&#95;result
</SyntaxTitle>
Bakes a transaction by sending an amount of tez with a parameter
         from the current source to another account. Returns the amount of
         gas consumed by the execution of the contract.


<SyntaxTitle syntax="cameligo">
val transfer&#95;exn : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p -&gt; tez -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer&#95;exn: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; (&#95;: p) =&gt; (&#95;: tez) =&gt; nat
</SyntaxTitle>
Bakes a transaction by sending an amount of tez with a parameter
        from the current source to another account. Returns the amount of
        gas consumed by the execution of the contract. Similar as
        `Test.transfer`, but fails when anything goes wrong.


<SyntaxTitle syntax="cameligo">
val get&#95;storage : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;s
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;storage: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; s
</SyntaxTitle>
Gets the storage of a typed account.


<SyntaxTitle syntax="cameligo">
val get&#95;balance : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;balance: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; tez
</SyntaxTitle>
Gets the balance of an account in tez.


<SyntaxTitle syntax="cameligo">
val to&#95;address : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;address: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; address
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val to&#95;contract : &#39;p &#39;s.(&#39;p, &#39;s) typed&#95;address -&gt; &#39;p contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;contract: &lt;p, s&gt;(&#95;: typed&#95;address&lt;p, s&gt;) =&gt; contract&lt;p&gt;
</SyntaxTitle>
Gets the contract corresponding to the default entrypoint of a
        typed address: the contract parameter in the result will be the
        type of the default entrypoint (generally `'param`, but this might
        differ if `'param` includes a "default" entrypoint).


<SyntaxTitle syntax="cameligo">
val get&#95;entrypoint : &#39;p &#39;s &#39;q.string -&gt; (&#39;p, &#39;s) typed&#95;address -&gt; &#39;q contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;entrypoint: &lt;p, s, q&gt;(&#95;: string) =&gt; (&#95;: typed&#95;address&lt;p, s&gt;) =&gt; contract&lt;q&gt;
</SyntaxTitle>
Gets the contract corresponding to an entrypoint of a typed
        address: the contract parameter in the result will be the type of
        the entrypoint, it needs to be annotated, entrypoint string should
        omit the prefix "%", but if passed a string starting with "%", it
        will be removed (and a warning emitted).
