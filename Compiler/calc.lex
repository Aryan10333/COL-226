structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
  
  
%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
\r       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
";" => (Tokens.TERM(!pos,!pos));
":" => (Tokens.COLON(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"->"      => (Tokens.ARROW(!pos,!pos));
"=>"      => (Tokens.GOES_TO(!pos,!pos));
"="      => (Tokens.ASSIGN(!pos,!pos));
"AND"      => (Tokens.AND(!pos,!pos));
"OR"      => (Tokens.OR(!pos,!pos));
"NOT" => (Tokens.NOT(!pos,!pos));
"XOR" => (Tokens.XOR(!pos,!pos));
"EQUALS" => (Tokens.EQUALS(!pos,!pos));
"IMPLIES" => (Tokens.IMPLIES(!pos,!pos));
"MINUS" => (Tokens.MINUS(!pos,!pos));
"PLUS" => (Tokens.PLUS(!pos,!pos));
"TIMES" => (Tokens.TIMES(!pos,!pos));
"NEGATE" => (Tokens.NEGATE(!pos,!pos));
"LESSTHAN" => (Tokens.LESSTHAN(!pos,!pos));
"GREATERTHAN" => (Tokens.GREATERTHAN(!pos,!pos));
"TRUE" => (Tokens.BIN_TRUE(!pos,!pos));
"FALSE" => (Tokens.BIN_FALSE(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"fn" => (Tokens.FN(!pos,!pos));
"fun" => (Tokens.FUN(!pos,!pos));
"then" => (Tokens.THEN(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"fi" => (Tokens.FI(!pos,!pos));
"let" => (Tokens.LET(!pos,!pos));
"in" => (Tokens.IN(!pos,!pos));
"end" => (Tokens.END(!pos,!pos));
"int" => (Tokens.INT(!pos,!pos));
"bool" => (Tokens.BOOL(!pos,!pos));
{digit}+ => (Tokens.CONST(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

