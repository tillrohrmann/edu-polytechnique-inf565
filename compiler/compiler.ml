open Ml_syntax
open StackMachine

let rec compileExpression exp = 
	match exp with
		| Boolean_constant b -> [Push (Bool b)]
		| Integer_constant i -> [Push (Int i)]
		| Variable id -> failwith "Program has to be transformed by De Bruijn transform.\n"
		| DeBruijn_variable (id,index) -> [Access index]
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
		| Function_application(f,a) -> compileExpression a@(compileExpression f)@[Apply]
		| Anonymous_function (p,d) -> [Cur ((compileExpression d)@[Return])]
		| Recursive_function (f,p,d,b) -> Rec ((compileExpression d)@[Return])::Let::compileExpression b@[Endlet]
		| Local_definition(id,d,b) -> compileExpression d@[Let]@compileExpression b@[Endlet]
		| If_then_else(c,t,e) -> 
			let thenCode = compileExpression t in
			let elseCode= compileExpression e in
			compileExpression c@[Branchneg ((List.length thenCode)+1)]@thenCode@[Branch (List.length elseCode)]@elseCode

let compileProgram = function
	| Program exp -> compileExpression exp