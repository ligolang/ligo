" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES WILL BE LOST.
if exists("b:current_syntax")
    finish
endif

syntax cluster top contains=TOP

" typeproduct
syntax region typeproduct start="{" end="}" contained contains=lowercaseidentifier,typeannotationfield,comma 

" typevariant
syntax region typevariant start="\[" end="\]" contained contains=comma,keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 

" typeint
syntax match typeint "\<[0-9]+\>" contained 
highlight link typeint Number 

" typeparentheses
syntax region typeparentheses start="(" end=")" contained contains=typefunparam,typeannotation,comma,keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 

" typename
syntax match typename "\<\([a-zA-Z_][a-zA-Z0-9_]*\)\>" contained 
highlight link typename Type 

" typefunparam
syntax match typefunparam "\<\([a-zA-Z$_][a-zA-Z0-9$_]*\)\>\s*:\@=" contained 
highlight link typefunparam Identifier 

" typegeneric
syntax region typegeneric start="<" end=">" contained contains=comma,keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 

" typeoperator
syntax match typeoperator "\(=>\|\.\||\)" contained 
highlight link typeoperator Operator 

" typeas
syntax region typeas matchgroup=typeas_ start="\<as\>" end="\(;\|%=\|]\|}\|+=\|\*=\|-=\|=\|\/=\|,\|:\|\<\(else\|default\|case\|as\)\>\)\@=" contains=keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 
highlight link typeas_ Keyword 

" typeannotationfield
syntax region typeannotationfield matchgroup=typeannotationfield_ start=":" end="\(,\|;\|}\)\@=" contained contains=keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 
highlight link typeannotationfield_ Operator 

" typeannotation
syntax region typeannotation matchgroup=typeannotation_ start=":" end="\()\|=>\|,\|{\|}\|=\|;\)\@=" contains=keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 
highlight link typeannotation_ Operator 

" typedefinition
syntax region typedefinition matchgroup=typedefinition_ start="\<type\>" end="\(;\|}\|@\|\<\(else\|default\|case\|type\|let\|const\|namespace\|interface\|export\|import\|function\)\>\)\@=" contains=keywords,uppercaseidentifier,typeoperator,typename,typeparentheses,typeint,typevariant,typeproduct,typebinder,typegeneric,string0,string1 
highlight link typedefinition_ Keyword 

" typebinder
syntax region typebinder start="<" end=">" contained contains=typename 

" objectproperty
syntax match objectproperty "\<\([a-zA-Z$_][a-zA-Z0-9$_]*\)\>\(\s*:\)\@=" contained nextgroup=objectproperty___ skipempty skipwhite
highlight link objectproperty Identifier 
syntax region objectproperty___ matchgroup=objectproperty____ start=":" end="\(,\|;\|}\)\@=" contained contains=@top 
highlight link objectproperty____ Operator 

" objectpropertystring
syntax match objectpropertystring "\".*\"\(\s*:\)\@=" contained nextgroup=objectpropertystring___ skipempty skipwhite
highlight link objectpropertystring String 
syntax region objectpropertystring___ matchgroup=objectpropertystring____ start=":" end="\(,\|;\|}\)\@=" contained contains=@top 
highlight link objectpropertystring____ Operator 

" objectpropertyint
syntax match objectpropertyint "\<[0-9]+\>\(\s*:\)\@=" contained nextgroup=objectpropertyint___ skipempty skipwhite
highlight link objectpropertyint Number 
syntax region objectpropertyint___ matchgroup=objectpropertyint____ start=":" end="\(,\|;\|}\)\@=" contained contains=@top 
highlight link objectpropertyint____ Operator 

" objectpropertyctor
syntax match objectpropertyctor "\<[A-Z][a-zA-Z0-9_$]*\>\(\s*:\)\@=" contained nextgroup=objectpropertyctor___ skipempty skipwhite
highlight link objectpropertyctor Label 
syntax region objectpropertyctor___ matchgroup=objectpropertyctor____ start=":" end="\(,\|;\|}\)\@=" contained contains=@top 
highlight link objectpropertyctor____ Operator 

" case
syntax region case matchgroup=case_ start="\<\(case\|default\)\>" matchgroup=case__ end=":" contains=@top 
highlight link case_ Conditional 
highlight link case__ Operator 

" parentheses
syntax region parentheses start="(" end=")" contains=typefunparam,comma,@top 

" objectorblock
syntax region objectorblock start="{" end="}" contains=objectpropertyctor,objectpropertyint,objectpropertystring,objectproperty,comma,@top 

" modulealias
syntax match modulealias_ "\<\([A-Z][a-zA-Z0-9_$]*\)\>" contained 
highlight link modulealias_ Structure 
syntax match modulealias "\<\(import\)\>" nextgroup=modulealias_ skipempty skipwhite
highlight link modulealias Conditional 

" moduleaccess
syntax match moduleaccess_ "\<\([a-zA-Z0-9_$]*\)\>" contained 
highlight link moduleaccess_ Identifier 
syntax match moduleaccess "\<[A-Z][a-zA-Z0-9_$]*\." contained nextgroup=moduleaccess_ skipempty skipwhite
highlight link moduleaccess Structure 

" lowercaseidentifier
syntax match lowercaseidentifier "\<[a-z$_][a-zA-Z0-9$_]*\>" contained 
highlight link lowercaseidentifier Identifier 

" uppercaseidentifier
syntax match uppercaseidentifier "\<[A-Z][a-zA-Z0-9_$]*\>" 
highlight link uppercaseidentifier Structure 

" whenclause
syntax region whenclause matchgroup=whenclause_ start="\<when\>" matchgroup=whenclause__ end=":" contains=@top 
highlight link whenclause_ Conditional 
highlight link whenclause__ Operator 

" ternary
syntax region ternary matchgroup=ternary_ start="?" matchgroup=ternary__ end=":" contains=@top 
highlight link ternary_ Operator 
highlight link ternary__ Operator 

" comma
syntax match comma "," contained 

" semicolon
syntax match semicolon ";" contained 

" operators
syntax match operators "\<\(-\|+\|%\|&&\||\||==\|!=\|<=\|>=\|<\|>\|\*\|/\|=\|!\|\*=\|/=\|%=\|+=\|-=\)\>" 
highlight link operators Operator 

" numericliterals
syntax match numericliterals "\<[0-9]+\(n\|tz\|tez\|mutez\|\)\>" 
highlight link numericliterals Number 

" controlkeywords
syntax match controlkeywords "\<\(switch\|if\|else\|for\|of\|while\|return\|break\|continue\|match\)\>" 
highlight link controlkeywords Conditional 

" keywords
syntax match keywords "\<\(export\|import\|from\|implements\|contract_of\|parameter_of\|function\|do\|namespace\|interface\|implements\|false\|true\)\>" 
highlight link keywords Keyword 

" letbinding
syntax match letbinding "\<\(let\|const\)\>" 
highlight link letbinding Keyword 

" macro
syntax match macro "^\#[a-zA-Z]\+" 
highlight link macro PreProc 

" attribute
syntax match attribute "\(@[a-zA-Z][a-zA-Z0-9_:.@%]*\)" 
highlight link attribute PreProc 

" string
syntax region string0 start="\"" end="\"" contains=@Spell 
highlight link string0 String 
syntax region string1 start="`" end="`" contains=@Spell 
highlight link string1 String 

" linecomment
syntax region linecomment start="\/\/" end="$" containedin=ALLBUT,blockcomment,string0,string1 contains=@Spell,attribute 
highlight link linecomment Comment 

" blockcomment
syntax region blockcomment start="/\*" end="\*/" containedin=ALLBUT,blockcomment,string0,string1 contains=@Spell,attribute 
highlight link blockcomment Comment 

let b:current_syntax = "jsligo"
