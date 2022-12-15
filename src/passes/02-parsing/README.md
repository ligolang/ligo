# Technical Documentation for Menhir specifications

## About Menhir

     http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
     http://gallium.inria.fr/~fpottier/menhir/manual.pdf

## Layout and Guidelines

When laying out rules for the same non-terminal, we use the closing
brace of a rule to separate it from the next by being on its own line,
like so:

     foo:
       .... { ...
       }
     | ... { ... }

When there are many rules for the same terminal, we present the rules
for the non-terminals involved in a left-right prefix manner (a.k.a
depth-first traversal in an algorithmic context). For example:

     foo:
       bar { ... }
     | baz { ... }

     bar:
       zoo { ... }

     zoo:
       A { ... }

     baz:
       B { ... }

When you change the grammar, take some time to see if you cannot
remove a reduction on error (`%on_error_reduce`) that is related to
your change.

Write comments. Inside them, escape text by writing it between square
brackets, following the `ocamldoc` convention.

Please avoid writing a leading vertical bar, like

     foo:
       | bar {}

The above is equivalent to

     foo:
       bar {}

but people could think it means

     for:
       {} | bar {}

because Menhir enables the sharing of semantic actions. (By the way,
the leading vertical bar is the only cause of an LR conflict in the
grammar of Menhir itself (personal communication to Rinderknecht by
Pottier, June 23, 2006).

We do not rely on predefined Menhir symbols, like [$symbolstartpos],
to help determine the regions (that is, source locations) of our
tokens and subtrees of our CST. One reason is that the semantic of
[$symbolstartpos] is that of ocamlyacc, and this does not blend well
with nullable prefixes of rules. That is why we use [Region.cover] to
compute the region in the source that corresponds to any given CST
node. This is more verbose than letting Menhir ask the lexer buffer
with an awkward semantic, but we are 100% in control.

A note on terminology: I sometimes use words taken from the context of
formal logic or programming theory. For example:

    https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions

We always define and write values of type [Region.t] by stating the
region first, like so: [{region; value}], and we always use name
punning when possible.

## Maintainance

  1. Download the `Makefile` at

     https://github.com/rinderknecht/OCaml-build

     Copy it or link to it from the parser directory.

     Create or check two hidden files needed by the `Makefile`:
     `.ParserMain.tag` (empty) and `.Parser.mly.tag` which should
     contain the following line:

     ```
     --table --explain --external-tokens Token --base Parser ParToken.mly
     ```

     Make sure there is a symbolic from `Parser.msg` to
     `errors.msg.in` or create it like so:

     ```
     ln -s errors.msg.in Parser.msg
     ```

     You can now update `Parser.msg` by running

     ```
     make msg
     ```

     If there is no `errors.msg.in` yet, because you are creating the
     parser, then run directly the previous command and copy
     `Parser.msg` to `errors.msg.in`.

     Apart syntax errors in the `.mly` file, two sorts of errors can
     be reported: LR conflicts and missing entries in `Parser.msg` (if
     the grammar has changed).

     For conflicts, there should be a file `Parser.conflicts` in the
     current directory (that is, the parser directory). If not, run
     the following command:

     ```
     menhir --table --explain --external-tokens Token --base Parser ParToken.mly
     ```

     To see the missing entries, open the file given in red by `make
     msg`: `.Parser.msg.new` (its location depends on your
     hardware). Errors are in `.Parser.msg.err`, in the same
     subdirectory as `.Parser.msg.new`.

  2. If there are no errors, you may be informed that `Parser.msg` has
     been automatically updated (to match `Parser.mly`) but the
     automaton states have changed:

     ```
     Warning: The LR items may have changed.
     > Check your error messages again.
     ```

     It may be the case that the new automaton is isomorphic to the
     previous one, but the messages for the error states can be wrong
     now. Check them again, starting with the error states about rules
     you have changed. If you already plan more changes that will
     likely yied the same message, you can delay reviewing the
     messages until you are at a stable state in your changes. This
     because of the enormous size of `Parser.msg`.

  3. Completely removing a token (that is, the `%token` declaration
     and the corresponding variant in `Token.ml`) can have extensive
     consequences on `Parser.msg` if that token was chosen before by
     Menhir to occur in many sentences characterising error
     states. (Error states are defined in `Parser.msg` by an input
     series of tokens.) In that case, it is best to review all those
     sentences. If the token was actually replaced by another, then
     replace the now missing token in the `Parser.msg`
     file. Otherwise, Menhir will ask you to remove the entries with
     the old token, and then to add them with the new token. (It does
     not know that it was replaced.)

  4. After `Parser.msg` has been updated, before writing error
     messages or reviewing their contents, check that no LR item needs
     a `%on_error_reduce` declaration, otherwise you would have to
     re-read all of them after adding it to `Parser.mly` (only in the
     relevant syntactic contexts, but it is a source of mistake, so it
     would be best to read *all again*).

  5. Then check `Parser.msg` for error states that could share the
     same message.

  6. When a production occurs in different syntactic contexts, like
     `nsepseq(type_expr,",")`, it is best to create more non-terminals
     to distinguish the context in the error states. For example,
     instead of

     ```
     type_tuple:
       brackets(nsepseq(type_expr,",")) { $1 }
     ```

     write:

     ```
     type_tuple:
       brackets(tuple_components) { $1 }

     tuple_components:
       nsepseq(component,",") { $1 }

     component:
       type_expr { $1 }
     ```

     This creates more error states, but this enables more precision
     by having separated syntactic contexts. Another example would be
     write:

     ```
     set_expr:
      compound("set",set_element) { $1 }

     set_element: expr { $1 }

     list_expr:
       compound("list",list_element) { $1 }

     list_element: expr { $1 }
     ```

     instead of inlining `set_element`and `list_element`:

     ```
     set_expr:
       compound("set",expr) { $1 }

     list_expr:
       compound("list",expr) { $1 }
     ```

     Here we have a tension between size of the Menhir specification
     and precision of error messages.

  7. For dual productions with attributes in prefix position, expect a
     duplication of error states with LR items about those
     productions. Like:

     ```
     contract: Ident As LBRACE Attr Ident ZWSP
     ##
     ## Ends in an error in state: 37.
     ##
     ## field_decl -> nseq(Attr) Ident . [ RBRACE COMMA ]
     ## field_decl -> nseq(Attr) Ident . type_annotation [ RBRACE COMMA ]
     ##
     ## The known suffix of the stack is as follows:
     ## nseq(Attr) Ident
     ##
     contract: Ident As LBRACE Ident ZWSP
     ##
     ## Ends in an error in state: 29.
     ##
     ## field_decl -> Ident . [ RBRACE COMMA ]
     ## field_decl -> Ident . type_annotation [ RBRACE COMMA ]
     ##
     ## The known suffix of the stack is as follows:
     ## Ident
     ##
     ```

     When the LR item in an error state has the period at the end of a
     production followed by many possible tokens, like

     ```
     contract: Ident As UIdent ZWSP
     ##
     ## Ends in an error in state: 19.
     ##
     ## module_access_t -> UIdent . DOT module_var_t [ VBAR SEMI RPAR RBRACKET RBRACE GT Else EQ EOF Default Case COMMA COLON As ARROW ]
     ## type_name -> UIdent . [ VBAR SEMI RPAR RBRACKET RBRACE LT GT Else EQ EOF Default Case COMMA COLON As ARROW ]
     ##
     ## The known suffix of the stack is as follows:
     ## UIdent
     ##
     ```

     you cannot write a meaningful message about the future.

     Usually, one would inform Menhir to reduce on that error state,
     thus to fail one transition later. Continuing this example, and
     assuming the rules

     ```
     core_type:
       ...
     | type_name             { TVar    $1 }
     | module_access_t       { TModA   $1 }
     | ...
     ```

     we would add

     ```
     %on_error_reduce type_name
     ```

     but because `type_name` occurs in many other contexts, we would
     lose precision elsewhere. It is better in that kind of instance
     to inline the production, like so:

     ```
     %inline
     type_name:
       "<ident>" | "<uident>" { $1 }
     ```

     After changing the grammar, and perhaps removing entries in
     `Parser.msg`, check `Parser.msg.new` for
     `". ["` so that you can add proper `%on_error_reduce`. If you
     forget, you may write error messages for states that won't be in
     error after adding `%on_error_reduce`.

  8. Optional non-terminals and error messages
     Consider the following grammatical rule:
     ```
        sequence:
          "begin" option(series) "end" { ... }
     ```
     It yields an error state with spurious reductions:
     ```
        In state XXX, spurious reduction of production option(series) -> last_expr
     ```
     The error message on the token `end` cannot know what the past
     was: "if the expression is complete" could be wrong if there was
     no expression at all. It is best to write:
     ```
        sequence:
          "begin" ioption(series) "end" { ... }
     ```

  9. After running `make msg` and assuming that it contains error messages    that are not the default one, you need to run

  ```
  make Parser_msg.ml
  ```

  10. When running `make Parser_msg.ml` and Menhir reports two sentences that cause an error in the same state. For instance:

```
File "Parser.msg", line 3007, characters 0-50:
File "Parser.msg", line 3024, characters 0-52:
Error: these sentences both cause an error in state 99.
```

If one of the sentences is the only one with a given error message, choose to delete the other one instead, or else save its message for later review. Note that the remaining messages may be wrong: you will review them all in a later phase anyway.

If Menhir reports lots of couple of sentences that cause an error in the same state, start with the end of the list and beware that if you run `make Parser_msg.ml` again, nothing will happen, so you will need to run first `make msg`. Since `Parser.mly` has not changed, there will be no need to reload it into your text editor.

Note that depending on which sentence you delete, its location in `Parser.msg` may overlap with one of two of another couple of sentences, forcing you to regenerate the list as above.

For example:

```
File "Parser.msg", line 2749, characters 0-39:
File "Parser.msg", line 2759, characters 0-44:
Error: these sentences both cause an error in state 120.
File "Parser.msg", line 2739, characters 0-29:
File "Parser.msg", line 2769, characters 0-34:
Error: these sentences both cause an error in state 437.
```

But those next two do not overlap:

```
File "Parser.msg", line 1578, characters 0-56:
File "Parser.msg", line 2703, characters 0-51:
Error: these sentences both cause an error in state 438.
File "Parser.msg", line 2713, characters 0-61:
File "Parser.msg", line 2723, characters 0-66:
```

In which case you deal first with the second couple of sentences first. Generally speaking, start with the non-overlapping couples the further down.

  11. Productions (rules) that end with an optional token are followed by the period of an LR item. They are a problem for determining the future when they characterise an error state, especially if a spurious reduction occured. For example,

```
interactive_expr: Record LBRACKET Ident SEMI While
##
## Ends in an error in state: 736.
##
## compound(Record,field_path_assignment) -> Record LBRACKET
sep_or_term_list(field_path_assignment,SEMI) . RBRACKET [ ... ]
```

where `sep_or_term_list(field_path_assignment,SEMI)` is a separated or
terminated list of `field_path_assignment`. It may be that the `SEMI`
(semicolon) was reduced *or not*: we cannot know. Therefore the error
message cannot propose it as a valid future.

  12. When done with `make Parser_msg.ml`, you can overwrite
      `errors.msg.in`with `Parser.msg` and `make clean` followed by
      `rm -f Parser.msg`. Then you can build the standalone parser
      with `dune build ParserMain.exe`.

  13. Reductions on error states is sometimes needed in order to end
   on a state where there is hopefully more right context to provide
   better error messages about possible futures. Practically, if, when
   examining the syntax error messages file, one comes accros an LR
   item of the form

   ```
     X -> some Things . [SOME TOKENS]
   ```

   where the next tokens between the square brackets are numerous
   and/or diverse in nature, then it is a good idea to add a clause

   ```
   %on_error_reduce X
   ```

   This will instruct the run-time generated by Menhir to reduce after
   the error, in the hope to find a state where it is easier to
   predict possible futures.

   Beware that the same item that would benefit from a reduction on
   error may occur in different states of the underlying LR
   automaton. In that case, priority will have to be specified by
   order of writing. When not using priorities, it is normally advised
   to list all the sentences to reduce in the same `%on_error_reduce`,
   but we do not do so, which make it is hard to remember when
   priority played a role, because Menhir only reports the number of
   states where priority of reduction played a role, but does not tell
   which ones. *)

  14. Inlining a production with `%inline` reduces the number of
      states in the generated LR automaton, but it may increase
      syntactical context of the inlined production, therefore
      improve the precision of syntax error messages. For instance:

      ```
      list_comprehension:
        brackets(comprehension_expr) { $1 }

      comprehension_expr:
        ...

      set_comprehension:
        braces(comprehension_expr) { $1 }
      ```
      is improved by inlining `comprehension_expr`: this way, the LR
      item will contain either `brackets(comprehension_expr) -> ...`
      or `braces(comprehension_expr) -> ...`, which makes it clear
      whether we are dealing with a list or a set, instead of
      `comprehension_expr -> ...` which could be either.
