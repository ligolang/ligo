---
id: test.io-reference
title: io
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val print : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
print: (&#95;: string) =&gt; unit
</SyntaxTitle>

Prints an string to stdout.

<SyntaxTitle syntax="cameligo">
val println : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
println: (&#95;: string) =&gt; unit
</SyntaxTitle>

Prints a string to stdout, ended with a newline.

<SyntaxTitle syntax="cameligo">
val eprint : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
eprint: (&#95;: string) =&gt; unit
</SyntaxTitle>

Prints a string to stderr.

<SyntaxTitle syntax="cameligo">
val eprintln : string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
eprintln: (&#95;: string) =&gt; unit
</SyntaxTitle>

Prints a string followed by a newline character to stderr.


<SyntaxTitle syntax="cameligo">
val log : &#39;a.&#39;a -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
log: &lt;a&gt;(&#95;: a) =&gt; unit
</SyntaxTitle>

Logs a value.


<SyntaxTitle syntax="cameligo">
val set&#95;test&#95;print : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
set&#95;test&#95;print: (&#95;: unit) =&gt; unit
</SyntaxTitle>

Turns on the printing of `test` prefixed values at the end of
tests. This is the default behaviour.


<SyntaxTitle syntax="cameligo">
val unset&#95;test&#95;print : unit -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
unset&#95;test&#95;print: (&#95;: unit) =&gt; unit
</SyntaxTitle>

Turns off the printing of `test` prefixed values at the end of tests.
