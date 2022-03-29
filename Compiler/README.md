Instructions:

1. Run the follwing command on the terminal to get "calc.lex.sml" file:
	ml-lex calc.lex

2. Run the follwing command on the terminal to get "calc.yacc.sml", "calc.yacc.desc" and "calc.yacc.sig" files:
	ml-yacc calc.yacc
	
3. On the SML interactive environment, run the command:
	use "loader.sml";
	
4. Check the testcases by running the command: 
	read_file("<file_name>");
	
where file_name is the test case file.
--> Note: Test case file should be in same directory as that of all program files.
