open Ml_syntax
  
open StackMachine

(**
	This function takes an expression of type program_expression and compiles it
	for the stack machine according to the problem description. The result is a
	list of instructions for the stack machine.
	
	@param exp variable of type program_exp specifying the AST
	
	@return commands list containing the compiled instructions for the stack machine
*) 
let rec compileExpression exp =
  match exp with
  | Boolean_constant b -> [ Push (Bool b) ]
  | Integer_constant i -> [ Push (Int i) ]
  | Variable id ->
      failwith "Program has to be transformed by De Bruijn transform.\n"
  | DeBruijn_variable (id, index) -> [ Access index ]
  | Binary (op, a, b) ->
      (compileExpression a) @
        ((compileExpression b) @
           (match op with
            | Plus -> [ Op MPlus ]
            | Minus -> [ Op MMinus ]
            | Mult -> [ Op MMult ]
            | Div -> [ Op MDiv ]
            | LAnd -> [ Op MLAnd ]
            | LOr -> [ Op MLOr ]
            | Equals -> [ Op MEquals ]
            | Greater -> [ Op MGreater ]))
	(*| Function_application (Keyword(Print_int), a) -> compileExpression a @ [CPrint_int]
	| Function_application (Keyword(Print_bool), b) -> compileExpression b @ [CPrint_bool]*)
  | Function_application (f, a) ->
			(* 1st: argument is placed on the stack. *)
			(* 2nd: function closure is loaded on the stack. *)
			(* 3rd: function call*)
      (compileExpression a) @ ((compileExpression f) @ [ Apply ])
  | Anonymous_function (p, d) ->
		(* Compile function body and combine it with the current environment *)
		(* Finally, return at the end of the function *)
		[ Cur ((compileExpression d) @ [ Return ]) ]
  | Recursive_function (f, p, d, b) ->
			(* Place a recursive definition in a special type Rec on the stack *)
			(* and add it to the environment. Then execute the following block*)
			(* before deleting the definition. *)
      ((Rec ((compileExpression d) @ [ Return ])) :: Let ::
        (compileExpression b)) @ [ Endlet ]
  | Local_definition (id, d, b) ->
      (compileExpression d) @
        ([ Let ] @ ((compileExpression b) @ [ Endlet ]))
	| Expression_block(e1,e2) ->
			(compileExpression e1) @ [ Pop ] @ (compileExpression e2)
	| Keyword(Print_int) -> [ Cur [Access 1;CPrint_int; Return ] ]
	| Keyword(Print_bool) -> [ Cur [Access 1;CPrint_bool; Return ] ]
	| Keyword(First) -> [ Cur [Access 1; CFirst; Return ] ]
	| Keyword(Second) -> [ Cur [Access 1; CSecond; Return ] ]
	| Pair(a,b) -> (compileExpression b) @ (compileExpression a) @ [ CPair ]
  | If_then_else (c, t, e) ->
      let thenCode = compileExpression t in
      let elseCode = compileExpression e
      in
        (compileExpression c) @
				(* Jump length: List.length thenCode + 1 because on adds an additional "Branch (List.length elseCode)" *)
          ([ Branchneg ((List.length thenCode) + 1) ] @
             (thenCode @ ([ Branch (List.length elseCode) ] @ elseCode)))
  
(** 
	This function unwraps the given program of type program to obtain the value
	of type program_exp representing the AST. This value is then compiled to obtain
	the list of instructions for the stack machine. 
	
	@param program of type program which represents the parsed program (AST)
	
	@return commands list representing the compiled program
*)
let compileProgram = function | Program exp -> compileExpression exp
  

