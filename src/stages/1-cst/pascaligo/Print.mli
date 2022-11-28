(* Printing a digest of the Concrete Syntax Tree (CST) as ASCII *)

(* This module prints the CST as a nice branching layout with some of
   the most relevant nodes and leaves decorated with source
   regions. This enables visually testing, for example, whether the
   parser properly enforces the expected associativity of a given
   operator. The printing of the CST can be requested on the
   command-line of the LIGO compiler with the command "print cst" (see
   file [src/bin/cli.ml]). *)

(* Local dependencies *)

module Tree = Cst_shared.Tree

(* IMPORTANT: If you add or remove a printing function, please mirror
   your changes in the aliases below accordingly. If you export
   functions printing special nodes of the CST, please call them
   "print_<node>_to_<dest>". *)

type ('src, 'dst) printer = Tree.state -> 'src -> 'dst

(* Printing nodes *)

val print_to_buffer : (CST.t, Buffer.t) printer
val print_to_string : (CST.t, string) printer
val print_pattern_to_string : (CST.pattern, string) printer

(* Aliases to be used fully qualified, e.g. [Print.to_buffer] *)

val to_buffer : (CST.t, Buffer.t) printer
val to_string : (CST.t, string) printer
val pattern_to_string : (CST.pattern, string) printer
