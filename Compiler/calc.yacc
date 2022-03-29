(* User  declarations *)
fun lookup "special" = true
  | lookup s = false

%%
(* required declarations *)
%name Calc

%term
  ID of string | ARROW | BIN_TRUE | BIN_FALSE | CONST of int | LET | IN | END | ASSIGN | MINUS | PLUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | FN | COLON | GOES_TO | INT | BOOL | FUN
| NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE | FI | RPAREN | LPAREN | TERM | EOF

%nonterm program of AST.program | statement of AST.exp | exp of AST.exp | bin_exp of AST.exp | non_if_exp of AST.exp | non_imply_exp of AST.exp | non_and_exp of AST.exp | non_op_exp of AST.exp | let_exp of AST.exp | decl of AST.decl | arithmetic_exp of AST.exp | non_sub_exp of AST.exp | non_plus_exp of AST.exp | non_times_exp of AST.exp | non_exp of AST.exp | comp_exp of AST.exp | bin_const of AST.exp | fn_exp of AST.exp | typ of AST.typ | non_arrow_typ of AST.typ | fun_app_exp of AST.exp | fun_exp of AST.exp
%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%left AND
%left OR
%left XOR
%nonassoc EQUALS
%left MINUS PLUS TIMES
%right NEGATE
%right ELSE IMPLIES NOT 
%right ARROW

  (* %nonassoc*)

%start program

%verbose

%%

program : statement program (AST.PGRM(statement, program))
      |    (AST.None)

statement : exp TERM (exp)
	| exp (exp)

  exp : bin_exp (bin_exp)
  	  | fun_app_exp (fun_app_exp)
  	  | fun_exp (fun_exp)

  bin_exp : IF non_if_exp THEN non_if_exp ELSE bin_exp FI (AST.ITEExp(AST.ITE, non_if_exp1, non_if_exp2, bin_exp))
  	  | non_if_exp (non_if_exp)
  	  
  fun_app_exp : ID exp (AST.AppExp(ID, exp))
  
  fun_exp : FUN ID LPAREN ID COLON typ RPAREN COLON typ GOES_TO exp (AST.Fun(ID1, ID2, typ1, typ2, exp))
  	  
  non_if_exp : non_imply_exp IMPLIES non_if_exp (AST.BinExp(AST.Imply, non_imply_exp, non_if_exp))
  	  | non_imply_exp (non_imply_exp)
  	  
  non_imply_exp : non_imply_exp AND non_and_exp (AST.BinExp(AST.And, non_imply_exp, non_and_exp))
  	  | non_imply_exp OR non_and_exp (AST.BinExp(AST.Or, non_imply_exp, non_and_exp))
	  | non_imply_exp XOR non_and_exp (AST.BinExp(AST.Xor, non_imply_exp, non_and_exp))
	  | non_and_exp (non_and_exp)
	  
  non_and_exp : NOT non_and_exp (AST.NotExp(AST.Not, non_and_exp))
  	  | non_op_exp (non_op_exp)
  	  
  non_op_exp : let_exp (let_exp)
  	  | arithmetic_exp (arithmetic_exp)
  	  | comp_exp (comp_exp)
  	  | bin_const (bin_const)
  
  bin_const : BIN_TRUE (AST.BoolExp(true))
  	  | BIN_FALSE (AST.BoolExp(false))
  
  let_exp : LET decl IN exp END (AST.LetExp(decl, exp))
  
  decl: ID ASSIGN exp (AST.VarDecl(ID, exp))
  	  | ID ASSIGN fn_exp (AST.VarDecl(ID, fn_exp))
  	  
  fn_exp : FN LPAREN ID COLON typ RPAREN COLON typ GOES_TO exp (AST.Fn(ID, typ1, typ2, exp))
  
  typ : non_arrow_typ ARROW typ (AST.Arrow(non_arrow_typ, typ))
  	  | non_arrow_typ (non_arrow_typ)
  
  non_arrow_typ : INT (AST.IntTyp)
  	  | BOOL (AST.BoolTyp)
  	  | LPAREN typ RPAREN (typ)
  
  comp_exp : arithmetic_exp LESSTHAN arithmetic_exp (AST.ArithExp(AST.Lessthan, arithmetic_exp1, arithmetic_exp2))
  	  | arithmetic_exp EQUALS arithmetic_exp (AST.ArithExp(AST.Equals, arithmetic_exp1, arithmetic_exp2))
  	  | arithmetic_exp GREATERTHAN arithmetic_exp (AST.ArithExp(AST.Greaterthan, arithmetic_exp1, arithmetic_exp2))
  
  arithmetic_exp : arithmetic_exp MINUS non_sub_exp (AST.ArithExp(AST.Minus, arithmetic_exp, non_sub_exp))
  	  | non_sub_exp (non_sub_exp)
  
  non_sub_exp : non_sub_exp PLUS non_plus_exp (AST.ArithExp(AST.Plus, non_sub_exp, non_plus_exp))
  	  | non_plus_exp (non_plus_exp)
  	  
  non_plus_exp : non_plus_exp TIMES non_times_exp (AST.ArithExp(AST.Times, non_plus_exp, non_times_exp))
  	  | non_times_exp (non_times_exp)
  
  non_times_exp : NEGATE non_times_exp (AST.NegateExp(AST.Negate, non_times_exp))
  	  | non_exp (non_exp)
  
  non_exp : CONST (AST.NumExp(CONST))
  	  | ID (AST.VarExp(ID))
  	  | LPAREN exp RPAREN (exp)
