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

and keywords=
	| Print_int
	| Print_bool
	| First
	| Second
	
and program_exp = 
	| Integer_constant of int
	| Boolean_constant of bool
	| Variable of identifier
	| DeBruijn_variable of identifier*int
	| Binary of operation*program_exp*program_exp
	| Function_application of program_exp*program_exp
	(* parameter name * function body *)
	| Anonymous_function of identifier*program_exp
	(* function name * parameter name * function body * following block *)
	| Recursive_function of identifier*identifier*program_exp*program_exp
	(* variable name * value definition * following block *)
	| Local_definition of identifier*program_exp*program_exp
	(* if condition * then block * else block *)
	| If_then_else of program_exp*program_exp*program_exp
	| Expression_block of program_exp*program_exp
	| Keyword of keywords
	| Pair of program_exp*program_exp
	
and program = Program of program_exp

