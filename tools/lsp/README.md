
To compile and run, the following tools are needed:

1) tree-sitter-clr (the Node.js one was usied during development)
2) optionally, nix package manager
3) haskell-stack

First, you need to generate the `parser.c`.
For that, do

```
cd tools/lsp/pascaligo
tree-sitter generate
```

Then
```
cd ../squirrel
stack install
```

To use the executable, do
```
squirrel <filename>.ligo
```

At this stage of development, it first prints the dump of tree-sitter parse tree, then its attempts to build actual parse tree.