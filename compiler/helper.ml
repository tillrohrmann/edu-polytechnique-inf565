open Ml_syntax
open Interpretation
open Types

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
	| Recursive_function (f,a,d,b) -> 
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
	
let prettyPrintAST ast = match ast 
	with 
	| Program _ -> printProgram 0 ast
	
let rec prettyPrintInterpretationResult indent result = 
	let indent_string = createIndent(indent) in
	match result with
	| Integer i -> print_string (indent_string^"Integer("^string_of_int i^")\n")
	| Boolean b -> print_string (indent_string^"Boolean("^string_of_bool b ^")\n")
	| Function_value (par,def,closure,recursive) -> print_string (indent_string^(if recursive then "rec " else "")^"Function_value(\n");
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
		
let prettyPrintExpType exp chrMode =
	let start = ref (Char.code 'a') in
	let map = Hashtbl.create 20 in
	let rec helper exp brackets= 
		match exp with
			| Bool -> print_string "bool"
			| Int -> print_string "int"
			| Function(a,b) -> 
				if brackets then print_string"(" else ();
				helper a true;
				print_string " -> ";
				helper b false;
				if brackets then print_string")" else ()
			| Type_variable i -> 
				if chrMode then
					let sign =
					(try Hashtbl.find map i 
					with 
						| _ -> 
							Hashtbl.replace map i (Char.chr !start); 
							let result = (Char.chr !start) in 
							start := !start +1;
							result)
					in
					print_string ("'"^Char.escaped sign)
				else
					print_string ("'"^string_of_int i)
			| Universal t -> helper t false
	in
	helper exp false;
	print_string "\n"
