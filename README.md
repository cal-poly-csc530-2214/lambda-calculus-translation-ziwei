Lambda Calculus Translation
-----------------------------------------------------
Assignment2.rkt contains the code to translate lambda calculus into valid Python code.  
The parse function takes in an S-expression and parses it into an abtract syntax tree.  
The toPython function takes in the AST and translates it into Python code, returning it as a string.  
The sToPython function combines the two, taking in an S-expression and converting it to Python code.
