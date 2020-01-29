%{
(* Grammar for boolean expressions in preprocessing directives of C# *)
%}

%token True False
%token <string> Ident
%token OR AND EQ NEQ NOT EOL LPAR RPAR

(* Entries *)

%start pp_expression
%type <Etree.t> pp_expression

%%

(* Grammar *)

pp_expression:
  e=pp_or_expression EOL { e }

pp_or_expression:
  e=pp_and_expression { e }
| e1=pp_or_expression OR e2=pp_and_expression {
    Etree.Or (e1,e2)
  }

pp_and_expression:
  e=pp_equality_expression { e }
| e1=pp_and_expression AND e2=pp_unary_expression {
    Etree.And (e1,e2)
  }

pp_equality_expression:
  e=pp_unary_expression { e }
| e1=pp_equality_expression EQ e2=pp_unary_expression {
    Etree.Eq (e1,e2)
  }
| e1=pp_equality_expression NEQ e2=pp_unary_expression {
    Etree.Neq (e1,e2)
  }

pp_unary_expression:
  e=pp_primary_expression   { e           }
| NOT e=pp_unary_expression { Etree.Not e }

pp_primary_expression:
  True                         { Etree.True     }
| False                        { Etree.False    }
| id=Ident                     { Etree.Ident id }
| LPAR e=pp_or_expression RPAR { e              }
