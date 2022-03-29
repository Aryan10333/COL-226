structure EVALUATOR  =
struct
	
	open AST
	open TypeEval

	val brokenTypes = Fail "Error in evaluation!"
	
	fun evalFile(e:program, env:environment, t_env:typEnv):value list = 
	let
		
		val t = getProgramType(e, t_env)
	
	in
		
		case e of 
			
			PGRM (e1, None) 			=> [evalExp(e1, env)]
		|	PGRM (Fun (f, x, typ1, typ2, e3), e2)	=> evalFile(e2, envAdd(f, FunVal (f, x, typ1, typ2, e3), env), typEnvAdd(f, getExpType(Fun (f, x, typ1, typ2, e3), t_env), t_env))
		|	PGRM (e1, e2)				=> (evalExp(e1, env))::evalFile(e2, env, t_env)
	
	end

	and evalExp(e:exp, env:environment):value =
	    case e of
		NumExp i                      	=> IntVal i
	      | BoolExp b         	   		=> BoolVal b
	      | VarExp x            	   		=> envLookup (x, env)
	      | ITEExp (ITE, e1, e2, e3)  		=> 
	        let
	        	
	        	val v1 = evalExp (e1, env)
	        
	        in
	        	
	        	case v1 of
	        		BoolVal true    => evalExp (e2, env)
	        	|	BoolVal false   => evalExp (e3, env)
       		|	_ 		=> raise brokenTypes  
	        
	        end
	         				  
	      | BinExp (b, e1, e2)  	              	=> evalBinExp(b, e1, e2, env)    
	      | NotExp (Not, e)     		      	=> 
	        let
	        
	        	val v = evalExp(e, env)
	        
	        in
	        
	        	case v of
	        		BoolVal true  	=> BoolVal false
	        	|	BoolVal false 	=> BoolVal true
	        	|	_ 		=> raise brokenTypes  
	        	
	        end
	        
	      | NegateExp (Negate, e) 		=> 	
	        let
	        
	        	val v = evalExp(e, env)
	        
	        in
	        
	        	case v of
	        		IntVal i  	=> IntVal (~i)
	        	|	_ 		=> raise brokenTypes  
	        	
	        end
	        
	      | ArithExp (oper, e1, e2) 		=> evalArithExp(oper, e1, e2, env)
	        
	      | LetExp(VarDecl(f, e1), e2)  		=>
	  	let
		    
		    val v1 = evalExp (e1, env)
		
		in
		    
		    evalExp(e2, envAdd (f, v1, env))
		
		end 		   
	      
	      | AppExp (f, e)				=> 
	        let
	        
	        	val e1 = envLookup (f, env)
	        	val v = evalExp(e, env)
	        
	        in
	        
	        	case (e1, v) of 
	        	
	        		(FnVal (x, typ1, typ2, e2), _)	=>	evalExp(e2, envAdd (x, v, env))
	        	|	(FunVal (x, y, typ1, typ2, e2), _)	=>	evalExp(e2, envAdd (y, v, env))
	        	|	(_, _)					=> raise Fail "Function expected."
	        
	        end
	        
	      | Fun (id1, id2, typ1, typ2, e)		=> FunVal (id1, id2, typ1, typ2, e)
	      | Fn (id, typ1, typ2, e)		=> FnVal (id, typ1, typ2, e)

	and
	evalBinExp(b:bin_binop, e1:exp, e2:exp, env:environment):value =
	case (b, evalExp(e1, env), evalExp(e2, env))  of
	    	(And, BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
	|   	(Or, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
	|   	(Xor, BoolVal b1, BoolVal b2) => BoolVal ((b1 andalso not(b2)) orelse (b2 andalso not(b1)))
	|   	(Imply, BoolVal b1, BoolVal b2) => BoolVal ((not(b1)) orelse b2)
	|   	_  => raise brokenTypes 
	
	and
	evalArithExp(oper:arith_binop, e1:exp, e2:exp, env:environment):value = 
	case (oper, evalExp(e1, env), evalExp(e2, env)) of 
		
		(Plus, IntVal i1, IntVal i2)	=> IntVal (i1 + i2)
	|	(Minus, IntVal i1, IntVal i2)	=> IntVal (i1 - i2)
	|	(Times, IntVal i1, IntVal i2)	=> IntVal (i1 * i2)
	|	(Lessthan, IntVal i1, IntVal i2)	=> BoolVal (i1 < i2)
	|	(Greaterthan, IntVal i1, IntVal i2)	=> BoolVal (i1 > i2)
	|	(Equals, IntVal i1, IntVal i2)	=> BoolVal (i1 = i2)
	|   	_  => raise brokenTypes

end
