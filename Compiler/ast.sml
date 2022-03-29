structure AST =
struct

type id = string

datatype not_op = Not

datatype ite_op = ITE

datatype term_op = Term

datatype arith_binop = Plus | Minus | Times | Lessthan | Greaterthan| Equals

datatype typ = IntTyp | BoolTyp | Arrow of typ * typ

datatype negate_op = Negate

datatype bin_binop = Imply | And | Or | Xor

datatype program = PGRM of exp * program | None

and decl = VarDecl of id * exp

and exp = ITEExp of ite_op * exp * exp * exp
	| BinExp of bin_binop * exp * exp
	| NotExp of not_op * exp
	| BoolExp of bool
	| VarExp of id
	| LetExp of decl * exp
	| ArithExp of arith_binop * exp * exp
	| NumExp of int
	| NegateExp of negate_op * exp
	| Fn of id * typ * typ * exp
	| AppExp of id * exp
	| Fun of id * id * typ * typ * exp
 
and value = IntVal of int
               | StringVal of string
	       | BoolVal of bool
	       | FunVal of id * id * typ * typ * exp
	       | FnVal of id * typ * typ * exp
	       
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail " Variable environment lookup error"
									    
end


