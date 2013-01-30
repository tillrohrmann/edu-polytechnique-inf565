open Ml_syntax
open Interpretation

let indentSign = "  "

let rec createIndent indent = 
	if indent <= 0 then
		""
	else
		indentSign^createIndent (indent-1)
		
let getOperationString = function
	| Plus -> "+"
	| Minus -> "-"
	| Mult -> "*"
	| Div -> "/"
	| LAnd -> "&&"
	| LOr -> "||"
	| Equals -> "="
	| Greater -> ">"

let rec printProgramExpression indent expression =
	let indent_string = createIndent indent in
	print_string indent_string;
	match expression with
	| Integer_constant i -> print_string ("Integer("^string_of_int i^")\n")
	| Boolean_constant b -> print_string ("Boolean("^string_of_bool b ^")\n")
	| Variable v -> print_string ("Variable("^v^")\n")
	| Binary (op,a,b) -> let operation_string = getOperationString op in
		print_string (operation_string^"(\n");
		printProgramExpression (indent+1) a;
		printProgramExpression (indent+1) b;
		print_string (indent_string^")\n")
	| Function_application (f,a) -> 
		print_string "Function application(\n";
		print_string (indent_string^indentSign^"Function(\n");
		printProgramExpression (indent+2) f;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^indentSign^"Argument(\n");
		printProgramExpression (indent+2) a;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^")\n")
	| Anonymous_function (arg,d) ->
		(*print_string "Anonymous function(\n";
		print_string (indent_string^indentSign^"Argument("^arg^")\n");
		print_string (indent_string^indentSign^"Definition(\n");
		printProgramExpression (indent+2) d;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^")\n")*)
		print_string ("fun "^arg^" ->\n");
		printProgramExpression (indent+1) d
	| Function (f,a,d,b) -> 
		(*print_string "Recursive Function(\n";
		print_string (indent_string^indentSign^"Name("^f^")\n");
		print_string (indent_string^indentSign^"Argument("^a^")\n");
		print_string (indent_string^indentSign^"Definition(\n");
		printProgramExpression (indent+2) d;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^indentSign^"Block(\n");
		printProgramExpression (indent+2) b;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^")\n")*)
		print_string ("Let rec "^f^" "^a^" =\n");
		printProgramExpression (indent+1) d;
		print_string (indent_string^"in\n");
		printProgramExpression (indent+1) b
	| Local_definition (v,d,b) ->
		print_string "Local definition(\n";
		print_string (indent_string^indentSign^"Name("^v^")\n");
		print_string (indent_string^indentSign^"Definition(\n");
		printProgramExpression (indent+2) d;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^indentSign^"Block(\n");
		printProgramExpression (indent+2) b;
		print_string (indent_string^indentSign^")\n");
		print_string (indent_string^")\n")
	| If_then_else (c,t,e) ->
		print_string "If(\n";
		printProgramExpression (indent+1) c;
		print_string (indent_string^")\n");
		print_string (indent_string^"then(\n");
		printProgramExpression (indent+1) t;
		print_string (indent_string^")\n");
		print_string (indent_string^"else(\n");
		printProgramExpression (indent+1) e;
		print_string (indent_string^")\n")
	| DeBruijn_variable (v,c) -> print_string ("Variable("^v^","^string_of_int c^")\n")

let rec printProgram indent = function
	| Program p -> 
		let indent_string = createIndent indent in
		print_string (indent_string^"Program(\n");
		printProgramExpression (indent+1) p;
		print_string (indent_string^")\n")
	| _ -> failwith "printProgram was expecting a node of type program\n"
	
let prettyPrintAST ast = match ast 
	with 
	| Program _ -> printProgram 0 ast
	| _ -> failwith "First node has to be of type program\n"
	
let rec prettyPrintInterpretationResult indent result = 
	let indent_string = createIndent(indent) in
	match result with
	| Integer i -> print_string (indent_string^"Integer("^string_of_int i^")\n")
	| Boolean b -> print_string (indent_string^"Boolean("^string_of_bool b ^")\n")
	| Function_value (par,def,closure) -> print_string (indent_string^"Function_value(\n");
		print_string (indent_string^indentSign^"fun "^par^" ->\n");
		printProgramExpression 2 def;
		print_string (indent_string^")\n");
		print_string (indent_string^"Closure(\n");
		let rec helper closure =
			match closure with
			| [] -> ()
			| h::t -> 
				prettyPrintInterpretationResult (indent+1) h; 
				helper t
		in
		helper closure;
		print_string (indent_string^")\n")
