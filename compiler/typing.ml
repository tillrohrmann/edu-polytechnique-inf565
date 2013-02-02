open Ml_syntax
open Types

let counter = ref 0

let getNewTypeVariable mapping = 
	let result = Type_variable !counter in
	Hashtbl.replace !mapping !counter result;
	counter := !counter+1;
	result
	
let identicalTypes a b = 
	a = b
	
let rec containedIn a b =
	match b with
		| Function (c,d) -> identicalTypes a c || identicalTypes a d || containedIn a c || containedIn a d
		| _ -> false

let rec union a b = 
	if identicalTypes a b then Some a else
	match a with
	| Type_variable c -> if containedIn a b = false then Some b else None
	| _ -> 
		(match b with 
			| Type_variable c -> if containedIn b a =false then Some a else None
			| _ -> None)

let getType mapping a =
	let rec helper old cur =
		match cur with
			| Type_variable i -> 
				let t = Hashtbl.find !mapping i in
				if t = old then
					old
				else
					let result = helper cur t in
					Hashtbl.replace !mapping i result;
					result
			| Function (p,b) -> Function(helper p p, helper b b)
			| _ -> cur
	in
	helper a a
	
let updateMapping mapping a b =
	match a with
		| Type_variable i -> Hashtbl.replace !mapping i b
		| _ -> ()
	
let rec unify mapping a b =
	let a_type = getType mapping a in
	let b_type = getType mapping b in
	let result = 
		match a_type with
		| Function(c,d) -> 
			(match b_type with
				| Function (e,f) -> 
					(match unify mapping c e with
						| Some t ->
							(match unify mapping d f with
								| Some tb -> Some(Function(t,tb))
								| None -> None)
						| None -> None)
				| _ -> union a_type b_type)
	  | _ -> union a_type b_type
	in
	(match result with
		| Some t ->
				updateMapping mapping a_type t;
				updateMapping mapping b_type t;
		| None -> ());
	result

let rec string_of_ml_types = function
	| Int -> "int"
	| Bool -> "bool"
	| Function (p,b) -> string_of_ml_types p ^" -> "^string_of_ml_types b
	| Type_variable c -> "'"^string_of_int c
	| Universal t -> string_of_ml_types t
	
let operation_type mapping op =
	match op with
	| Plus -> Function(Int,Function(Int,Int))
	| Minus ->Function(Int,Function(Int,Int))
	| Mult ->Function(Int,Function(Int,Int))
	| Div ->Function(Int,Function(Int,Int))
	| LAnd ->Function(Bool,Function(Bool,Bool))
	| LOr ->Function(Bool,Function(Bool,Bool))
	| Greater ->Function(Int,Function(Int,Bool))
	| Equals ->
		let typeVar = getNewTypeVariable mapping in
		Function(typeVar,Function(typeVar,Bool))
		
let rec envContainsType env t = 
	match env with
		| [] -> false
		| h::tail -> if h = t then true else envContainsType tail t
		
let generalizeType env env_rec mapping t =
	let typeVarMapping = Hashtbl.create 20 in
	let rec helper = function
		| Universal t -> helper t
		| Function(p,b) -> Function (helper p, helper b)
		| Type_variable i -> 
			if envContainsType env (Type_variable i) || envContainsType env_rec (Type_variable i) then
				Type_variable i
			else
			(try Hashtbl.find typeVarMapping i with
				| _ -> 
					let newTypeVar = getNewTypeVariable mapping in
					Hashtbl.replace typeVarMapping i newTypeVar;
					newTypeVar )
		| _ as x -> x
	in
	helper t

let rec expression_type env env_rec mapping exp = 
	match exp with
	| Boolean_constant _ -> Bool
	| Integer_constant _ -> Int
	| Variable v -> failwith "Program has to be transformed to use De Bruijn indices\n"
	| DeBruijn_variable (v,i) -> 
		(match getType mapping (List.nth env (i-1)) with
			| Universal t -> generalizeType env env_rec mapping t
			| _ as r -> r)
	| Anonymous_function (p,d) -> 
		let newTypeVar = getNewTypeVariable mapping in
		let result = expression_type (newTypeVar::env) env_rec mapping d in
		Function(getType mapping newTypeVar,result)
	| Recursive_function (f,p,d,b) -> 
		let funType = getNewTypeVariable mapping in
		let parType = getNewTypeVariable mapping in
		let resultFunType = expression_type (parType::funType::env) (env_rec) mapping d in
		expression_type (Universal(Function (getType mapping parType,resultFunType))::env) (env_rec) mapping b
	| Function_application (f,a) -> 
		let funType = expression_type env env_rec mapping f in
		let argumentType = expression_type env env_rec mapping a in
		let newTypeVar = getNewTypeVariable mapping in
		let result = unify mapping funType (Function(argumentType,newTypeVar)) in
		(match result with
			| Some t -> getType mapping newTypeVar
			| None -> 
				(match funType with
					| Function(p,f) -> failwith ("The argument has type "^string_of_ml_types argumentType^" but an expression of type "^string_of_ml_types p^" was expected")
					| _ -> failwith ("Could not unify type "^string_of_ml_types funType^" and "^string_of_ml_types (Function(argumentType,newTypeVar)))))
	| Binary (op,a,b) -> 
		let op_type = operation_type mapping op in
		let a_type = expression_type env env_rec mapping a in
		let b_type = expression_type env env_rec mapping b in
		(match op_type with
		| Function (p,b) -> 
			(match unify mapping p a_type with
			| Some ta ->
				(match b with
				| Function (p,b) -> 
					(match unify mapping p b_type with
					| Some tb -> b
					| None -> failwith ("The argument has type "^string_of_ml_types b_type^" but an expression of type "^string_of_ml_types p^" was expected."))
				| _ -> failwith ("The function of type "^string_of_ml_types op_type^" is applied to too many arguments."))
			| None -> failwith ("The argument has type "^string_of_ml_types a_type^" but an expression of type "^string_of_ml_types p^" was expected."))
		| _ -> failwith "The expression is not a function.")
	| Local_definition (v,d,b) -> 
		let v_type = expression_type env env_rec mapping d in
		expression_type ((Universal v_type)::env) (env_rec) mapping b
	| If_then_else (c,t,e) ->
		let condition_type = expression_type env env_rec mapping c in
		if condition_type != Bool then
			failwith ("Condition has type "^string_of_ml_types condition_type^" but an expression of type "^string_of_ml_types Bool^" was expected.")
		else
			let then_type = expression_type env env_rec mapping t in
			let else_type = expression_type env env_rec mapping e in
			match unify mapping then_type else_type with
			| Some t -> t
			| None -> failwith ("Else expression has type "^string_of_ml_types else_type^" but an expression of type "^string_of_ml_types then_type^" was expected.")

let program_type prog =
	let mapping = ref (Hashtbl.create 20) in
	match prog with
		| Program exp -> expression_type [] [] mapping exp