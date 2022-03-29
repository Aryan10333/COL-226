Testcases
===

Correct Input Case
---

The testcases "himym.csv" covers most intricacies of a normal CSV file including Quotes, Newlines, Delimiters present 
inside Fields and empty Fields. I have also added sample output when the delimiter is converted from comma to semicolon 
("himym.ssv"), amperstand ("himym.ssv") and vertical bar ("himym.vsv"). This testcase gives an example of a correct 
input file.


How to use
===
1. Open terminal and run command "rlwrap sml" to get into sml interactive environment. 
2. Run command: use "assign1.sml" to load functions in it.
3. Use functions appropriately:
	convertDelimiters(infilename, delim1, outfilename, delim2);				Eg. convertDelimiters(infilename, #",", outfilename, #"+");
	csv2tsv(infilename, outfilename);
	tsv2csv(incilename, outfilename);
