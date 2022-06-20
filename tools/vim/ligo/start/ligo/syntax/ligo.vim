if exists("b:current_syntax")
    finish
endif

" string
syntax region string start="\"" end="\"" 
highlight link string String 

" comment
syntax match comment "\/\/.*$" 
syntax region comment start="(\*" end="\*)" 
highlight link comment Comment 

" typeint
syntax match typeint "\<[0-9]+\>" contained 
highlight link typeint Number 

" typemodule
syntax match typemodule "\<\([A-Z][a-zA-Z0-9_$]*\)\." contained 
highlight link typemodule Identifier 

" typeparentheses
syntax region typeparentheses start="(" end=")" contained contains=ofkeyword,identifierconstructor,typeproduct,typeoperator,typename,typeparentheses,typemodule,typeint,string 

" typename
syntax match typename "\<\([a-z_][a-zA-Z0-9_]*\)\>" contained 
highlight link typename Type 

" typeoperator
syntax match typeoperator "\(->\|\.\||\|\*\)" contained 
highlight link typeoperator Operator 

" typeproduct
syntax match typeproduct "\<record\>" contained nextgroup=typeproduct___ 
highlight link typeproduct Keyword 
syntax match typeproduct___ "\s*" contained nextgroup=typeproduct______ 
syntax region typeproduct______ start="\[" end="\]" contained contains=identifier,typeannotationfield,semicolon 

" typeannotationfield
syntax region typeannotationfield matchgroup=typeannotationfield_ start="\(:\)" end="\(;\|\]\)\@!" contained contains=ofkeyword,identifierconstructor,typeproduct,typeoperator,typename,typeparentheses,typemodule,typeint,string 
highlight link typeannotationfield_ Operator 

" typeannotation
syntax region typeannotation matchgroup=typeannotation_ start="\(:\)" end="\(;\|)\|}\|\<is\>\|=\|:=\)\@!" contains=ofkeyword,identifierconstructor,typeproduct,typeoperator,typename,typeparentheses,typemodule,typeint,string 
highlight link typeannotation_ Operator 

" typedefinition
syntax region typedefinition matchgroup=typedefinition_ start="\<\(type\)\>" end="\(\<\(type\|recursive\|module\|function\|end\|const\)\>\|;\|{\|^#\|\[@\)\@!" contains=iskeyword,ofkeyword,identifierconstructor,typeproduct,typeoperator,typename,typeparentheses,typemodule,typeint,string 
highlight link typedefinition_ Keyword 

" constorvar
syntax match constorvar "\<\(const\|var\)\>" 
highlight link constorvar Keyword 

" identifierconstructor
syntax match identifierconstructor "\<\([A-Z][a-zA-Z0-9_$]*\)\>" 
highlight link identifierconstructor Label 

" identifier
syntax match identifier "\<[a-zA-Z$_][a-zA-Z0-9$_]*\>" contained 

" module
syntax match module_ "[a-z_][a-zA-Z0-9_$]*" contained 
highlight link module_ Identifier 
syntax match module "\<\([A-Z][a-zA-Z0-9_$]*\)\." nextgroup=module_ 
highlight link module Structure 

" iskeyword
syntax match iskeyword "\<\(is\)\>" contained 
highlight link iskeyword Keyword 

" ofkeyword
syntax match ofkeyword "\<\(of\)\>" contained 
highlight link ofkeyword Keyword 

" semicolon
syntax match semicolon ";" contained 

" operators
syntax match operators "\<\(-\|+\|/\|mod\|land\|lor\|lxor\|lsl\|lsr\|&&\|||\|<\|>\|=/=\|<=\|>=\)\>" 
highlight link operators Operator 

" numericliterals
syntax match numericliterals "\<[0-9]+\(n\|tz\|tez\|mutez\|\)\>" 
highlight link numericliterals Number 

" function
syntax match function_ "\<[a-zA-Z$_][a-zA-Z0-9$_]*\>" contained 
highlight link function_ Statement 
syntax match function "\(function\)\W" nextgroup=function_ 
highlight link function Keyword 

" controlkeywords
syntax match controlkeywords "\<\(case\|with\|if\|then\|else\|assert\|failwith\|begin\|end\|in\|is\|from\|skip\|block\|contains\|to\|step\|of\|while\|for\|remove\)\>" 
highlight link controlkeywords Conditional 

" macro
syntax match macro "^\#[a-zA-Z]\+" 
highlight link macro PreProc 

" attribute
syntax match attribute "\[@.*\]" 
highlight link attribute PreProc 

let b:current_syntax = "ligo"