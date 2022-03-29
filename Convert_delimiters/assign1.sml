exception doubleQuotesMissing;
exception emptyInputFile;

fun read_file (infile:string) =       (*Taken from scanner.sml*)
let 
   	
   	val instream = TextIO.openIn infile;
	
	fun loop instream =
		
		case TextIO.inputLine instream of
	    	SOME line => line :: loop instream
    	    | NONE      => []
    	
in
	 		
	loop instream before TextIO.closeIn instream
    
end;
  	

fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
let
	
	val outfile = TextIO.openOut outfilename;
	
	val data = read_file(infilename);
	
	fun makeChanges(s,outfile,field,o_quote,c_quote,e_quote) = 
	let
	
		val characters = String.explode(s);
								
		fun trace([],l,open_quote,close_quote,f,end_quote) = (f,open_quote,close_quote,end_quote)
			| trace(x::ls,l,open_quote,close_quote,f,end_quote) = 
			
				if ((ord x = ord delim1) andalso (open_quote = 0 andalso close_quote = 0)) then (if (end_quote = 1) then TextIO.output(outfile,String.implode(f @ [#"\""] @ [delim2])) (*"--"c or --c*)
																								
																								else TextIO.output(outfile,String.implode(f @ [delim2])); 
																								
																								trace(ls,l @ [delim2],0,0,[],0))
																								
				else if ((ord x = ord delim1) andalso (open_quote = 1 andalso close_quote = 0)) then trace(ls,l @ [delim1],1,0,f @ [delim1],0)
								
				else if ((ord x = ord #"\"") andalso (open_quote = 0 andalso close_quote = 0)) then trace(ls,l @ [x],1,0,f @ [x],0)
					
				else if ((ord x = ord #"\"") andalso (open_quote = 1 andalso close_quote = 0)) then (if (ord(hd(ls)) = ord #"\"") then trace(tl(ls),l @ [x] @ [x],1,0,f @ [x] @ [x],0) else trace(ls,l @ [x],0,0,f @ [x],0))
							
				else if ((ord x = ord delim2) andalso (open_quote = 0 andalso close_quote = 0)) then (if (end_quote = 0) then trace(ls,#"\""::l @ [x],0,0,#"\""::f @ [x],1) 
																									else trace(ls,l @ [x],0,0,f @ [x],1)) (*Special case*)
								
				else if ((ord x = ord delim2) andalso (open_quote = 1 andalso close_quote = 0)) then trace(ls,l @ [x],1,0,f @ [x],0)
				
				else if ((ord x = ord #"\n") andalso (open_quote = 0 andalso close_quote = 0)) then (if (end_quote = 0) then TextIO.output(outfile,String.implode(f @ [x])) (*Special case*)
																								
												else TextIO.output(outfile,String.implode(f @ [#"\""] @ [x])); 
																								
												trace(ls,l @ [x],0,0,[],0))
												
				else if ((ord x = ord #"\n") andalso (open_quote = 1 andalso close_quote = 0)) then trace(ls,l @ [#"\n"],1,0,f @ [#"\n"],0)
								
				else trace(ls,l @ [x],open_quote,close_quote,f @ [x],end_quote);
		
		
		
	
	in
	
		if (null field) then trace(characters,[],0,0,[],0) else trace(characters,[],1,0,field,0)
	
	end;
		
	fun read([],"",field,o_quote,c_quote,e_quote) = TextIO.closeOut outfile
		| read([],s,field,o_quote,c_quote,e_quote) = 
		let
		
			val (new_field,open_quote,close_quote,end_quote) = makeChanges(s,outfile,field,o_quote,c_quote,e_quote)
			
		in
				
			if (null new_field) then TextIO.closeOut outfile else raise doubleQuotesMissing
		
		end				
							
		| read(l::ls,x,field,o_quote,c_quote,e_quote) = 
		let
		
			val (new_field,open_quote,close_quote,end_quote) = makeChanges(x,outfile,field,o_quote,c_quote,e_quote)
			
		in
		
			read(ls,l,new_field,open_quote,close_quote,end_quote)
		
		end
  	
in
        
	if (null data) then raise emptyInputFile else read(tl(data),hd(data),[],0,0,0)
  	
end;


fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");
fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename,#"\t",outfilename,#",");

