type identifier = string

and function_name = Function_name of identifier

and operation =
	| Plus
	| Minus
	| Div
	| Mult
	| LAnd
	| LOr
	| Equals
	| Greater
	
and program_exp = 
	| Integer_constant of int
	| Boolean_constant of bool
	| Variable of identifier
	| DeBruijn_variable of identifier*int
	| Binary of operation*program_exp*program_exp
	| Function_application of program_exp*program_exp
	| Anonymous_function of identifier*program_exp
	| Recursive_function of identifier*identifier*program_exp*program_exp
	| Local_definition of identifier*program_exp*program_exp
	| If_then_else of program_exp*program_exp*program_exp
	
and program = Program of program_exp

