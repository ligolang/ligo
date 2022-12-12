" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES WILL BE LOST.
if exists("b:current_syntax")
    finish
endif

syntax cluster top contains=TOP

" typeproduct
syntax region typeproduct start="{" end="}" contained contains=uppercaseidentifier,typeannotation,semicolon 

" typeint
syntax match typeint "\<[0-9]+\>" contained 
highlight link typeint Number 

" typeparentheses
syntax region typeparentheses start="(" end=")" contained contains=uppercaseidentifier,ofkeyword,typeoperator,typename,typevar,typeparentheses,typeint,typeproduct,string 

" typevar
syntax match typevar "'\<[a-z_][a-zA-Z0-9_]*\>" contained 
highlight link typevar Type 

" typename
syntax match typename "\<[a-z_][a-zA-Z0-9_]*\>" contained 
highlight link typename Type 

" typeoperator
syntax match typeoperator "\(->\|\.\|\*\||\)" contained 
highlight link typeoperator Operator 

" typeannotationlambda
syntax region typeannotationlambda matchgroup=typeannotationlambda_ start=":" end="\()\|=\|;\|}\|->\)\@=" contained contains=uppercaseidentifier,ofkeyword,typeoperator,typename,typevar,typeparentheses,typeint,typeproduct,string 
highlight link typeannotationlambda_ Operator 

" typeannotation
syntax region typeannotation matchgroup=typeannotation_ start=":" end="\()\|=\|;\|}\)\@=" contains=uppercaseidentifier,ofkeyword,typeoperator,typename,typevar,typeparentheses,typeint,typeproduct,string 
highlight link typeannotation_ Operator 

" typedefinition
syntax region typedefinition matchgroup=typedefinition_ start="\<type\>" end="\(^#\|\[%\|\<\(let\|in\|type\|end\|module\)\>\|)\)\@=" contains=uppercaseidentifier,ofkeyword,typeoperator,typename,typevar,typeparentheses,typeint,typeproduct,string 
highlight link typedefinition_ Keyword 

" lowercaseidentifier
syntax match lowercaseidentifier "\<[a-z$_][a-zA-Z0-9$_]*\>" contained 
highlight link lowercaseidentifier Identifier 

" uppercaseidentifier
syntax match uppercaseidentifier "\<[A-Z][a-zA-Z0-9_$]*\>" 
highlight link uppercaseidentifier Structure 

" lambda
syntax region lambda matchgroup=lambda_ start="\<fun\>" matchgroup=lambda__ end="\(->\)" contains=typeannotationlambda 
highlight link lambda_ Statement 
highlight link lambda__ Operator 

" ofkeyword
syntax match ofkeyword "\<\(of\)\>" contained 
highlight link ofkeyword Keyword 

" semicolon
syntax match semicolon ";" contained 

" operators
syntax match operators "::\|-\|+\|/\|\<\(mod\|land\|lor\|lxor\|lsl\|lsr\)\>\|&&\|||\|<\|>\|<>\|<=\|>=" 
highlight link operators Operator 

" numericliterals
syntax match numericliterals "\<[0-9]+\(n\|tz\|tez\|mutez\|\)\>" 
highlight link numericliterals Number 

" letbinding
syntax match letbinding__ "\<\([a-zA-Z$_][a-zA-Z0-9$_]*\)\>" contained 
highlight link letbinding__ Statement 
syntax match letbinding_ "\<rec\>\|" contained nextgroup=letbinding__ skipempty skipwhite
highlight link letbinding_ StorageClass 
syntax match letbinding "\<\(let\)\>" nextgroup=letbinding_ skipempty skipwhite
highlight link letbinding Keyword 

" moduledeclaration
syntax match moduledeclaration "\<module\>" 
highlight link moduledeclaration Keyword 

" structurekeywords
syntax match structurekeywords "\<\(struct\|end\|in\)\>" 
highlight link structurekeywords Keyword 

" controlkeywords
syntax match controlkeywords "\<\(match\|with\|if\|then\|else\|assert\|failwith\|begin\)\>" 
highlight link controlkeywords Conditional 

" macro
syntax match macro "^\#[a-zA-Z]\+" 
highlight link macro PreProc 

" attribute
syntax match attribute "\[@.*\]" 
highlight link attribute PreProc 

" string
syntax region string start="\"" end="\"" contains=@Spell 
highlight link string String 

" linecomment
syntax region linecomment start="\/\/" end="$" containedin=ALLBUT,string,blockcomment contains=@Spell 
highlight link linecomment Comment 

" blockcomment
syntax region blockcomment start="(\*" end="\*)" containedin=ALLBUT,string,linecomment contains=@Spell 
highlight link blockcomment Comment 

let b:current_syntax = "mligo"
