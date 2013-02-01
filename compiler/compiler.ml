open Ml_syntax
open StackMachine

| Integer_constant of int
	| Boolean_constant of bool
	| Variable of identifier
	| DeBruijn_variable of identifier*int
	| DeBruijn_variable_rec of identifier*int
	| Binary of operation*program_exp*program_exp
	| Function_application of program_exp*program_exp
	| Anonymous_function of identifier*program_exp
	| Recursive_function of identifier*identifier*program_exp*program_exp
	| Local_definition of identifier*program_exp*program_exp
	| If_then_else of program_exp*program_exp*program_exp
	

let rec compileExpression exp = 
	match exp with
		| Boolean_constant b -> [Push (Bool b)]
		| Integer_constant i -> [Push (Int i)]
		| Variable id -> failwith "Program has to be transformed by De Bruijn transform.\n"
		| DeBruijn_variable (id,index) -> [Access index]
		| DeBruijn_variable_rec(id,index) -> failwith "Not supported yet.\n"
		| Binary (op,a,b) -> compileExpression a@compileExpression b@
			(match op with
				| Plus -> [Op MPlus]
				| Minus -> [Op MMinus]
				| Mult -> [Op MMult]
				| Div -> [Op MDiv]
				| LAnd -> [Op MLAnd]
				| LOr -> [Op MLOr]
				| Equals -> [Op MEquals]
				| Greater -> [Op MGreater])
		| Function_application(f,a) -> compileExpression a@[Cur (compileExpression f); Apply]
		| Anonymous_function (p,d) -> compileExpression d
		| Recursive_function (f,p,d,b) -> failwith "Not supported yet.\n"
		| Local_definition(id,d,b) -> failwith "Not supported yet.\n"
		| If_then_else(c,t,e) -> 

let compileProgram = function
	| Program exp -> compileExpression exp