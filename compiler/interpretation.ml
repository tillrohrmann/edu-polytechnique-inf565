open Ml_syntax

(**
	Type for representing the interpretation results.
	
	Function_value: Representing a function type
	
	@param 1st: parameter name
	@param 2nd: function body
	@param 3nd: closure
	@param 4nd: true => recursive, false => not recursive
*)
type interpretation_result =
  | Boolean of bool
  | Integer of int
  | Function_value of string * program_exp * interpretation_result list
                      * bool
	| IUnit

(**
	Unwrap method to retrieve an int from a variable of the form Integer i
*)
let get_integer a =
  match a with | Integer i -> i | _ -> failwith "a is not an integer"

(**
	Unwrap method to retrieve an bool from a variable of the form Boolean b
*)
let get_boolean a =
  match a with | Boolean b -> b | _ -> failwith "a is not a boolean"

(**
	Testing function whether the variable a is of the form Integer i
*)
let check_integer a = match a with | Integer i -> true | _ -> false
  
(**
	Testing function whether the variable a is of the form Boolean b
*)	
let check_boolean a = match a with | Boolean b -> true | _ -> false

(**
	Testing function whether the variabl f is of the form Function_value par def closure recursive
*)
let check_function f =
  match f with
  | Function_value (par, def, closure, recursive) -> true
  | _ -> false

(**
	This function adds the two parameters if they are integers.
	Otherwise the operation will fail.
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@result a+b of type int
*)
let plus a b =
  if (check_integer a) && (check_integer b)
  then (get_integer a) + (get_integer b)
  else failwith "Plus requires two operands of type int"

(**
	This function subtracts the second from the first parameter if they are integers.
	Otherwise the operation will fail.
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a-b of type int
*)
let minus a b =
  if (check_integer a) && (check_integer b)
  then (get_integer a) - (get_integer b)
  else failwith "Minus requires two operands of type int"

(**
	This function multiplies the given parameters if they are integers.
	Otherwise the operation will fail.
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a*b of type int
*)
let mult a b =
  if (check_integer a) && (check_integer b)
  then (get_integer a) * (get_integer b)
  else failwith "Mult requires two operands of type int"

(**
	This function divides the first parameter by the second parameter if they are both integers.
	Otherwise the operation will fail. There is no check for a division by 0.
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a/b of type int
*)
let div a b =
  if (check_integer a) && (check_integer b)
  then (get_integer a) / (get_integer b)
  else failwith "Div requires two operands of type int"

(**
	This function performs the logical and operation on its parameters if they are both bools.
	Otherwise the operation will fail.
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a&&b of type bool
*)
let landop a b =
  if (check_boolean a) && (check_boolean b)
  then (get_boolean a) && (get_boolean b)
  else failwith "Logical and requires two operands of type int"

(**
	This function performs the logical or operation on its parameters if they are both bools. 
	Otherwise the operation will fail. 
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a||b of type bool
*) 
let lorop a b =
  if (check_boolean a) && (check_boolean b)
  then (get_boolean a) || (get_boolean b)
  else failwith "Logical or requires two operands of type int"

(**
	This function checks its two parameters on semantic equality. 
	
	@param a 1st operand
	@param b 2nd operand
	
	@return bool
*) 
let equals a b = a = b

(**
	This function checks whether the first parameter is greater than the second if both parameters
	are integers. Otherwise the operation will fail. 
	
	@param a variable of type interpretation_result
	@param b variable of type interpretation_result
	
	@return a > b of type bool
*) 
let greater a b =
  if (check_integer a) && (check_integer b)
  then (get_integer a) > (get_integer b)
  else failwith "Greater requires two operands of type int"

(**
	This function takes an operation of type Ml_syntax.operation and two operands of
	type interpretation_result and performs the operation on them. 
	
	@param op operation of type Ml_syntax.operation
	@param a operand of type interpretation_result
	@param b operand of type interpretation_result
	
	@return operation result
*)
let binary_operation op a b =
  match op with
  | Plus -> Integer (plus a b)
  | Minus -> Integer (minus a b)
  | Mult -> Integer (mult a b)
  | Div -> Integer (div a b)
  | LAnd -> Boolean (landop a b)
  | LOr -> Boolean (lorop a b)
  | Equals -> Boolean (equals a b)
  | Greater -> Boolean (greater a b)

(**
	This function calculates the closure of a function. This is achieved by calculating
	which De Bruijn indices are not found in the context of the function call. This is
	simply the max (De Bruijn index - number of binding places up to the variable access)
	of all found De Bruijn indices.
	
	@param boundedThreshold current number of found binding places
	@param 2nd expression of type program_expression
	
	@return number of variables of the current context which have to be included in
		the closure for the given expression
*)
let rec calcClosureDeBruijn boundedThreshold =
  function
  | Boolean_constant b -> 0
  | Integer_constant b -> 0
  | Variable v -> 0
  | DeBruijn_variable (v, i) -> i - boundedThreshold
  | Anonymous_function (p, d) -> 
		(* new binding place -> increment threshold *)
		calcClosureDeBruijn (boundedThreshold + 1) d
  | Local_definition (i, d, b) ->
      max (calcClosureDeBruijn boundedThreshold d)
			(* new binding place -> increment threshold *)
        (calcClosureDeBruijn (boundedThreshold + 1) b)
  | If_then_else (c, t, e) ->
      max (calcClosureDeBruijn boundedThreshold c)
        (max (calcClosureDeBruijn boundedThreshold t)
           (calcClosureDeBruijn boundedThreshold e))
  | Function_application (f, a) ->
      max (calcClosureDeBruijn boundedThreshold f)
        (calcClosureDeBruijn boundedThreshold a)
  | Recursive_function (f, p, d, b) ->
			(* 2 new binding places -> increment threshold by 2 *)
      max (calcClosureDeBruijn (boundedThreshold + 2) d)
			(* new binding place -> increment threshold *)
        (calcClosureDeBruijn (boundedThreshold + 1) b)
  | Binary (op, a, b) ->
      max (calcClosureDeBruijn boundedThreshold a)
        (calcClosureDeBruijn boundedThreshold b)
	| Expression_block(e1,e2) ->
			max (calcClosureDeBruijn boundedThreshold e1)
				(calcClosureDeBruijn boundedThreshold e2)
	| Keyword(_) -> 0

(**
	This function retrieves the first number elements of the list state. 
	
	@param state a list
	@param number specifies how many elements from the beginning of the list
		shall be copied
		
	@return first number elements of the list state 
*)
let rec head state number =
  if number = 0
  then []
  else (match state with | [] -> [] | h :: t -> h :: (head t (number - 1)))
 
(**
	This function realizes the interpretation of a program represented by an AST. 
	For that purpose the function maintains a list representing the current state. 
	The elements in that list correspond to the De Bruijn indices in the current
	context. Furthermore, it maintains a list of possible closure values which
	is used for the evaluation of partial functions.
	
	@param prog of type program representing the program (AST)
	
	@return result of type interpretation_result
*)
let interprete prog =
  let rec helper state closure =
    function
    | Boolean_constant b -> Boolean b
    | Integer_constant i -> Integer i
		| Keyword(_) as keyword -> Function_value("x",keyword,closure,false)
    | Variable v ->
        failwith "Program has to be transformed to use De Bruijn indices"
    | DeBruijn_variable (v, i) -> 
			(* Access i-1 th element of the state list *)
			List.nth state (i - 1)
    | Anonymous_function (a, d) -> 
			(* Non recursive function with the current closure *)
			Function_value (a, d, closure, false)
    | Local_definition (i, d, b) ->
			(* Evaluate the definition d and append it to the state list before*)
			(* interpreting the block b. *)
			helper ((helper state [] d) :: state) [] b
    | If_then_else (c, t, e) ->
        let result = helper state [] c
        in
          (match result with
           | Boolean b -> if b then helper state [] t else helper state [] e
           | _ -> failwith "If condition has to have a boolean type")
		| Function_application (Keyword(Print_int), a) ->
			let aValue = helper state [] a in
			(match aValue with
				| Integer i -> print_string ((string_of_int i)^"\n"); IUnit
				| _ -> failwith("print_int requires argument of type int"))
		| Function_application(Keyword(Print_bool), a) ->
			let aValue = helper state [] a in
			(match aValue with
				| Boolean true -> print_string "true\n" ; IUnit
				| Boolean false -> print_string "false\n"; IUnit
				| _ -> failwith("print_bool requires argument of type bool"))
    | Function_application (f, a) ->
        let functionValue = helper state [] f
        in
          (match functionValue with
           | Function_value (par, def, closure, false) ->
							(* evaluate the function argument *)
               let aValue = helper state [] a
               in
								(* append value of function argument and closure at the front of the state list *)
								(* as well as to the current closure list for the case that this is only a partial *)
								(* function application. *)
                 helper ((aValue :: closure) @ state) (aValue :: closure) def
           | (Function_value (par, def, closure, true) as f) ->
               let aValue = helper state [] a
               in
								(* if the function is additionally recursive, then add the function value itself *)
								(* to the state list and the closure list in order to be able to make recursive *)
								(* calls. *)
                 helper ((aValue :: f :: closure) @ state)
                   (aValue :: f :: closure) def
           | _ -> failwith "Function_application expects a function type")
    | Recursive_function (f, p, d, b) ->
				(* calc closure for the recursive definition d *)
        let closureThreshold = calcClosureDeBruijn 2 d
        in
					(* create a function value consisting of the definition d and the closure and *)
					(* append it at the front of the state list. Then interprete the block b. *)
          helper
            ((Function_value (p, d, (head state closureThreshold), true)) ::
              state)
            [] b
		| Expression_block(e1,e2) ->
				ignore (helper state closure e1); helper state closure e2
    | Binary (op, a, b) ->
        binary_operation op (helper state [] a) (helper state [] b)
  in match prog with | Program exp -> helper [] [] exp
  

