open Ml_syntax

type interpretation_result = Boolean of bool | Integer of int | Function_value of string*program_exp*(interpretation_result list)*bool

let get_integer a =
	match a with
	| Integer i -> i
	| _ -> failwith "a is not an integer"
	
let get_boolean a =
	match a with
	| Boolean b -> b
	| _ -> failwith "a is not a boolean"

let check_integer a =
	match a with
	| Integer i -> true
	| _ -> false
	
let check_boolean a =
	match a with
	| Boolean b -> true
	| _ -> false
	
let check_function f = 
	match f with
	| Function_value (par,def,closure,recursive) -> true
	| _ -> false
	
let plus a b =
	if check_integer a && check_integer b then
		get_integer a + get_integer b
	else
		failwith "Plus requires two operands of type int"
		
let minus a b =
	if check_integer a && check_integer b then
		get_integer a - get_integer b
	else
		failwith "Minus requires two operands of type int"
		
let mult a b =
	if check_integer a && check_integer b then
		get_integer a * get_integer b
	else
		failwith "Mult requires two operands of type int"
		
let div a b =
	if check_integer a && check_integer b then
		get_integer a / get_integer b
	else
		failwith "Div requires two operands of type int"
		
let landop a b =
	if check_boolean a && check_boolean b then
		get_boolean a && get_boolean b
	else
		failwith "Logical and requires two operands of type int"
		
let lorop a b =
	if check_boolean a && check_boolean b then
		get_boolean a || get_boolean b
	else
		failwith "Logical or requires two operands of type int"
		
let equals a b =
	a = b

let greater a b=
	if check_integer a && check_integer b then
		get_integer a > get_integer b
	else
		failwith "Greater requires two operands of type int"

let binary_operation op a b =
	match op with
	| Plus -> Integer(plus a b)
	| Minus ->Integer( minus a b)
	| Mult -> Integer(mult a b)
	| Div -> Integer(div a b)
	| LAnd -> Boolean(landop a b)
	| LOr -> Boolean(lorop a b)
	| Equals -> Boolean(equals a b)
	| Greater -> Boolean(greater a b)

let rec calcClosureDeBruijn boundedThreshold = function
	| Boolean_constant b -> 0
	| Integer_constant b -> 0
	| Variable v -> 0
	| DeBruijn_variable (v,i) -> i-boundedThreshold
	| Anonymous_function(p,d) -> calcClosureDeBruijn (boundedThreshold+1) d
	| Local_definition(i,d,b) -> max (calcClosureDeBruijn boundedThreshold d) (calcClosureDeBruijn (boundedThreshold+1) b)
	| If_then_else(c,t,e) -> max (calcClosureDeBruijn boundedThreshold c) (max (calcClosureDeBruijn boundedThreshold t) (calcClosureDeBruijn boundedThreshold e))
	| Function_application(f,a) -> max (calcClosureDeBruijn boundedThreshold f) (calcClosureDeBruijn boundedThreshold a)
	| Recursive_function(f,p,d,b) -> max (calcClosureDeBruijn (boundedThreshold+2) d) (calcClosureDeBruijn (boundedThreshold+1) b)
	| Binary(op,a,b) -> max (calcClosureDeBruijn boundedThreshold a) (calcClosureDeBruijn boundedThreshold b)

let rec head state number = 
	if number = 0 then 
		[]
	else
		match state with
			| [] -> []
			| h::t -> h::(head t (number-1))
		
let interprete prog = 
	let rec helper state closure = function
		| Boolean_constant b -> Boolean(b)
		| Integer_constant i -> Integer(i)
		| Variable v -> failwith "Program has to be transformed to use De Bruijn indices"
		| DeBruijn_variable (v,i) -> List.nth state (i-1)
		| Anonymous_function (a,d) -> Function_value(a,d,closure,false)
		| Local_definition (i,d,b) -> 
			helper ((helper state [] d)::state) [] b
		| If_then_else (c,t,e) -> 
			let result = helper state [] c in
			(match result with
			| Boolean b -> if b then helper state [] t else helper state [] e
			| _ -> failwith "If condition has to have a boolean type")
		| Function_application (f,a) -> 
			let functionValue = helper state [] f in
			(match functionValue with
			| Function_value (par,def,closure,false) ->
				let aValue = helper state [] a in
				helper (aValue::closure@state) (aValue::closure) def
			| Function_value (par,def,closure,true) as f ->
				let aValue = helper state [] a in
				helper (aValue::f::closure@state) (aValue::f::closure) def
			| _ -> failwith "Function_application expects a function type")
		| Recursive_function(f,p,d,b) -> 
			let closureThreshold = calcClosureDeBruijn 2 d in
			helper (Function_value(p,d,head state closureThreshold,true)::state) [] b
		| Binary(op,a,b) -> binary_operation op (helper state [] a) (helper state [] b)
	in
	match prog with
	| Program exp -> helper [] [] exp
