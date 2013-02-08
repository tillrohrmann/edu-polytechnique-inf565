open Ml_syntax
  
open Interpretation
  
open Types

(* String which is used for indentation *)
let indentSign = "  "

(** 
	Create the indentation string for the a given level indent. 
	
	@param indent specifies the indentation level 
	
	@return indentation string
*) 
let rec createIndent indent =
  if indent <= 0 then "" else indentSign ^ (createIndent (indent - 1))

(** 
	Get for the operations of type Ml_syntax.operation the string representation. 
	
	@param operation
	
	@return string representing the given operation
*)
let getOperationString =
  function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | LAnd -> "&&"
  | LOr -> "||"
  | Equals -> "="
  | Greater -> ">"

(**
	This function pretty prints a given expression of type program_expression at a given
	indentation level. 
	
	@param indent indentation level (int)
	@param expression of type program_expression which shall be pretty printed 
*)
let rec printProgramExpression indent expression =
  let indent_string = createIndent indent
  in
    (print_string indent_string;
     match expression with
     | Integer_constant i ->
         print_string ("Integer(" ^ ((string_of_int i) ^ ")\n"))
     | Boolean_constant b ->
         print_string ("Boolean(" ^ ((string_of_bool b) ^ ")\n"))
     | Variable v -> print_string ("Variable(" ^ (v ^ ")\n"))
     | Binary (op, a, b) ->
         let operation_string = getOperationString op
         in
           (print_string (operation_string ^ "(\n");
            printProgramExpression (indent + 1) a;
            printProgramExpression (indent + 1) b;
            print_string (indent_string ^ ")\n"))
     | Function_application (f, a) ->
         (print_string "Function application(\n";
          print_string (indent_string ^ (indentSign ^ "Function(\n"));
          printProgramExpression (indent + 2) f;
          print_string (indent_string ^ (indentSign ^ ")\n"));
          print_string (indent_string ^ (indentSign ^ "Argument(\n"));
          printProgramExpression (indent + 2) a;
          print_string (indent_string ^ (indentSign ^ ")\n"));
          print_string (indent_string ^ ")\n"))
     | Anonymous_function (arg, d) ->
         (print_string ("fun " ^ (arg ^ " ->\n"));
          printProgramExpression (indent + 1) d)
     | Recursive_function (f, a, d, b) ->
         (print_string ("Let rec " ^ (f ^ (" " ^ (a ^ " =\n"))));
          printProgramExpression (indent + 1) d;
          print_string (indent_string ^ "in\n");
          printProgramExpression (indent + 1) b)
     | Local_definition (v, d, b) ->
         (print_string "Local definition(\n";
          print_string
            (indent_string ^ (indentSign ^ ("Name(" ^ (v ^ ")\n"))));
          print_string (indent_string ^ (indentSign ^ "Definition(\n"));
          printProgramExpression (indent + 2) d;
          print_string (indent_string ^ (indentSign ^ ")\n"));
          print_string (indent_string ^ (indentSign ^ "Block(\n"));
          printProgramExpression (indent + 2) b;
          print_string (indent_string ^ (indentSign ^ ")\n"));
          print_string (indent_string ^ ")\n"))
     | If_then_else (c, t, e) ->
         (print_string "If(\n";
          printProgramExpression (indent + 1) c;
          print_string (indent_string ^ ")\n");
          print_string (indent_string ^ "then(\n");
          printProgramExpression (indent + 1) t;
          print_string (indent_string ^ ")\n");
          print_string (indent_string ^ "else(\n");
          printProgramExpression (indent + 1) e;
          print_string (indent_string ^ ")\n"))
     | DeBruijn_variable (v, c) ->
         print_string
           ("Variable(" ^ (v ^ ("," ^ ((string_of_int c) ^ ")\n")))))

(**
	This function prints a program ( represented as an AST ) at a given indentation level. 
	
	@param indent indentation level
	@param 2nd program of type program
*)
let rec printProgram indent =
  function
  | Program p ->
      let indent_string = createIndent indent
      in
        (print_string (indent_string ^ "Program(\n");
         printProgramExpression (indent + 1) p;
         print_string (indent_string ^ ")\n"))

(**
	Pretty print of a program represented as an AST. 
	
	@param ast of type program
*)
let prettyPrintAST ast = match ast with | Program _ -> printProgram 0 ast

(**
	Pretty print of the interpretation result computed by the interpretation
	function. 
	
	@param indent indentation level
	@param result interpretation result of type interpretation_result
*)
let rec prettyPrintInterpretationResult indent result =
  let indent_string = createIndent indent
  in
    match result with
    | Integer i ->
        print_string
          (indent_string ^ ("Integer(" ^ ((string_of_int i) ^ ")\n")))
    | Boolean b ->
        print_string
          (indent_string ^ ("Boolean(" ^ ((string_of_bool b) ^ ")\n")))
    | Function_value (par, def, closure, recursive) ->
        (print_string
           (indent_string ^
              ((if recursive then "rec " else "") ^ "Function_value(\n"));
         print_string
           (indent_string ^ (indentSign ^ ("fun " ^ (par ^ " ->\n"))));
         printProgramExpression 2 def;
         print_string (indent_string ^ ")\n");
         print_string (indent_string ^ "Closure(\n");
         let rec helper closure =
           (match closure with
            | [] -> ()
            | h :: t ->
                (prettyPrintInterpretationResult (indent + 1) h; helper t))
         in (helper closure; print_string (indent_string ^ ")\n")))

(**
	This function pretty prints a type expression. 
	
	@param exp type expression of type ml_types
	@param chrMode boolean which decides whether type variables shall be 
		represented by numbers or by characters. In the latter case the variables
		are only in the scope of the given expression unique.
*)
let prettyPrintExpType exp chrMode =
  let start = ref (Char.code 'a') in
  let map = Hashtbl.create 20 in
  let rec helper exp brackets =
    match exp with
    | Bool -> print_string "bool"
    | Int -> print_string "int"
    | Function (a, b) ->
        (if brackets then print_string "(" else ();
				(* a parameter can be of a function type, thus we have to print it with brackets *)
         helper a true;
         print_string " -> ";
         helper b false;
         if brackets then print_string ")" else ())
    | Type_variable i ->
        if chrMode
        then
          (let sign =
						(* check whether we have already replaced i by an character *)
             try Hashtbl.find map i
             with
             | _ ->
								(* if not, then insert it into the hash map *)
                 (Hashtbl.replace map i (Char.chr !start);
                  let result = Char.chr !start
                  in (start := !start + 1; result))
           in print_string ("'" ^ (Char.escaped sign)))
        else print_string ("'" ^ (string_of_int i))
    | Universal t -> helper t false
  in (helper exp false; print_string "\n")
  

