%{

  open AST
  open Exp

%}

(* Worked in the same room as Rhen Daffin, another student in your class.  We never looked at each other's code or anything like that, but worked in the same vecinity with the same white board*)

%token <int32> INTCONST
%token <float> FLOATCONST
%token <bool> BOOLCONST
%token <string> ID

%token DEF LET WHILE IF THEN ELSE REF INT FLOAT BOOL UNIT TT IN

%token NOT DEREF
%token PLUS MINUS TIMES DIV
%token AND OR LT INT_EQ DEFEQ

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ COLON COMMA EOF 

(* AS NEEDED, ADD ASSOCIATIVITY/PRECEDENCE DIRECTIVES HERE, AS 
   DESCRIBED IN THE ASSIGNMENT TEXT ON THE COURSE WEBSITE *) 

%right SEMI

%left OR
%left AND
%nonassoc INT_EQ LT 
%left PLUS MINUS
%left TIMES DIV
%nonassoc unary_over_binary

%start <(AST.ty, unit Exp.exp) AST.prog> prog

%%

mytype:
| LPAREN ty = mytype RPAREN { ty }
(* ADD PRODUCTIONS HERE FOR GRUMPY'S TYPES *)
| INT               { TyInt }
| FLOAT             { TyFloat }
| BOOL              { TyBool }
| ty = mytype REF   {TyRef ty}
| UNIT              { TyUnit }

unop:
| MINUS   { UMinus }
| NOT     { UNot }
| DEREF   { UDeref }

%inline binop:
| PLUS    { BPlus }
(* ADD PRODUCTIONS HERE FOR GRUMPY'S BOOLEAN OPERATORS *)    
| MINUS   { BMinus }
| TIMES   { BTimes }
| DIV     { BDiv }
| AND     { BAnd }
| OR      { BOr }
| LT      { BLt }
| INT_EQ  { BIntEq }
| DEFEQ   { BUpdate }

id:
| i = ID  { Id i }

exp_list:
| l = separated_list(COMMA, exp) { l }
  
exp:
(* ADD ONE PRODUCTION HERE FOR THE NONTERMINAL raw_exp *)  
| e = raw_exp { 
    {start_of = $startpos;
     end_of = $endpos; 
     exp_of = e; 
     ety_of = () } 
   }
| LPAREN e = exp RPAREN { e }

raw_exp:
(* ADD PRODUCTIONS HERE FOR GRUMPY'S EXPRESSION CONSTRUCTORS *)  
| n = INTCONST                              { EInt n }
| n = FLOATCONST                            { EFloat n }
| n = id                                    { EId n }
| x = exp SEMI y = exp                      { exp_seq x y }
| x = id LPAREN y = exp_list RPAREN         { ECall(x, y) }
| REF n = exp                               { ERef n }
| x = unop y = exp %prec unary_over_binary  { EUnop(x, y) }
| x = exp y = binop z = exp                 { EBinop(y, x, z) }
| IF x = exp THEN y = exp ELSE z = exp      { EIf(x, y, z) }
| LET x = id EQ y = exp IN z = exp          { ELet(x, y, z) }
| LBRACE n = exp RBRACE                     { EScope n }
| TT                                        { EUnit }
| n = BOOLCONST                             { if n then ETrue else EFalse }
| WHILE x = exp LBRACE y = exp RBRACE       { EWhile(x, y) }


(* THE FOLLOWING NONTERMINALS/PRODUCTIONS ARE GIVEN FOR YOU: *)  
arg:
| arg = id COLON arg_ty = mytype
  { mk_tid arg arg_ty }         
  
arg_list:
| l = separated_list(COMMA, arg)
  { l }       
  
fundef:
| DEF nm = id LPAREN args = arg_list RPAREN COLON ret_ty = mytype LBRACE body = exp RBRACE { {nm; args; ret_ty; body} }
(* END GIVEN *)
  
(* FIX THE PROG RULE BELOW: *)

prog:
| x = list(fundef) y = exp EOF {
  {
    fundefs = x;
    result = y}
  }