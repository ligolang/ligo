all: build generate

build:
	dune build . LigoSyntaxHighlighting.exe

generate:
	../../_build/default/tools/ligo-syntax-highlighting/LigoSyntaxHighlighting.exe --vim $(realpath ../vim/ligo/start/ligo) --emacs $(realpath ../emacs) --vscode $(realpath ../vscode/syntaxes) --textmate $(realpath ../ligo-syntax-highlighting/textmate)
