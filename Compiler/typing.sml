structure TypeEval = 	(*Taken help from class lecture 27th April*)
struct

	open AST
	
	type typEnv = (id * typ) list
	
	val brokenTyp = Fail "Type Error!"
	
	fun typEnvAdd(var:id, t:typ, env:typEnv):typEnv = (var, t)::env

	fun typEnvLookup(var:id, env:typEnv):typ = 
		case List.find(fn (x, _) => x = var) env of
			SOME (x, v)	=> v
		|	NONE		=> raise Fail ("Variable " ^ var ^ " is without a type.")
		
	fun getProgramType(e:program, env:typEnv):typ =
		case e of
			PGRM(e1, None)				=> getExpType(e1, env)
		|	PGRM (Fun (f, x, typ1, typ2, e3), e2)	=> getProgramType(e2, typEnvAdd(f, getExpType(Fun (f, x, typ1, typ2, e3), env), env))
		|	PGRM (e1, e2)	=> 
			let
				val t1 = getExpType(e1,env)	
			in
				getProgramType(e2, env)	
			end
	
	and getExpType(e:exp, env:typEnv):typ = 
		case e of
			NumExp i			=> IntTyp
		|	BoolExp b    	   		=> BoolTyp
	      	| 	VarExp x      	   		=> typEnvLookup (x, env)
	      	| 	Fun (id1, id2, typ1, typ2, e)	=> 
	      		let
	      			
	      			val t = getExpType(e, typEnvAdd(id1, Arrow(typ1, typ2), typEnvAdd(id2, typ1, env)))
	      		
	      		in
	      		
	      			if (t <> typ2) then raise Fail ("Exp of function " ^ id1 ^ " is not matching with the assigned type.") else Arrow(typ1, typ2)
	      		
	      		end
	      		
	      	| 	Fn (id, typ1, typ2, e)		=> 
	      		let
	      			
	      			val t = getExpType(e, typEnvAdd(id, typ1, env))
	      		
	      		in
	      		
	      			if (t <> typ2) then raise Fail ("Fn exp return type is not matching with the assigned type.") else Arrow(typ1, typ2)
	      		
	      		end
	      	
	      	| 	ITEExp (ITE, e1, e2, e3)  	=> 
	      		let
	      			
	      			val t1 = getExpType(e1, env)
	      		
	      		in
	      			if(t1 <> BoolTyp) then raise Fail ("Condition in if-then-else statement is not of BoolTyp.") 
	      			else
	      				let
	      					
	      					val t2 = getExpType(e2, env)
	      					val t3 = getExpType(e3, env)
	      				in
	      					
	      					if (t2 <> t3) then raise Fail ("Then and else parts are not of same types.") else t2
	      				
	      				end 
	      					
	      		end
	      	
	      	|	BinExp (b, e1, e2) 		=> 
	      		let
	      		
	      			val t1 = getExpType(e1, env)
				val t2 = getExpType(e2, env)

			in
				
				if ((t1 = BoolTyp) andalso (t2 = BoolTyp)) then BoolTyp else raise Fail ("Operands of the BoolExp are not of BoolTyps.")				
			
			end
			
		| 	NotExp (Not, e)	      	=> 
			let
			
				val t = getExpType(e, env)
			
			in
			
				if (t <> BoolTyp) then raise Fail ("Not op is not operating on exp of BoolTyp.") else BoolTyp			
			
			end
		
		|	NegateExp (Negate, e) 		=> 
			let
			
				val t = getExpType(e, env)
			
			in
			
				if (t <> IntTyp) then raise Fail ("Negate op is not operating on exp of IntTyp.") else BoolTyp			
			
			end
		
		| 	ArithExp (oper, e1, e2) 		=> 
			let
	      		
	      			val t1 = getExpType(e1, env)
				val t2 = getExpType(e2, env)

			in
				
				if ((t1 = IntTyp) andalso (t2 = IntTyp)) then 
					case oper of
						Plus 		=> IntTyp
					|	Minus		=> IntTyp
					|	Times		=> IntTyp
					|	Lessthan	=> BoolTyp
					|	Greaterthan	=> BoolTyp
					|	Equals		=> BoolTyp
				
				
				else raise Fail ("Operands of the ArithExp are not of IntTyp.")				
			
			end
		
		|	LetExp(VarDecl(f, e1), e2)  		=> getExpType(e2, typEnvAdd(f, getExpType(e1, env), env))
		
		| 	AppExp (f, e)				=> 
			case (getExpType(VarExp f, env), getExpType(e, env)) of
				(Arrow(t1, t2), t3)	=> if (t1 = t3) then t2 else raise Fail ("Argument type and parameter type are not matching for function " ^ f ^ ".")
			|	(_, _)			=> raise Fail ("Function expected.")
						
end
