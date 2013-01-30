open Ml_syntax

let rec getIndex indices v = 
	match indices with
	| [] -> 0
	| (id,count)::t -> if v = id then count else getIndex t v
	
let addIdentifier indices v =
	let rec helper indices v result found =
		match indices with
		| [] -> if found = true then result else (v,1)::result
		| (id,count)::t -> if id = v then helper t v ((id,1)::result) true else helper t v ((id,count+1)::result) false
	in
	helper indices v [] false

let conditionalTransform prog =
	let rec conditionalTransformHelper = function
		| Variable v -> Variable v
		| DeBruijn_variable (v,c) -> DeBruijn_variable(v,c)
		| Integer_constant i -> Integer_constant i
		| Boolean_constant b -> Boolean_constant b
		| Function_application (f,a) -> Function_application (conditionalTransformHelper f,conditionalTransformHelper a)
		| Anonymous_function (i,d) -> Anonymous_function (i,conditionalTransformHelper d)
		| Function (f,p,d,b) -> Function(f,p,conditionalTransformHelper d, conditionalTransformHelper b)
		| Local_definition (i,d,b) -> Local_definition(i,conditionalTransformHelper d, conditionalTransformHelper b)
		| If_then_else (c,t,e) -> If_then_else(conditionalTransformHelper c, conditionalTransformHelper t, conditionalTransformHelper e)
		| Binary (op,a,b) ->
			let ac = conditionalTransformHelper a in
			let bc = conditionalTransformHelper b in
			match op with
			| LAnd -> 
				(match ac with
				| Boolean_constant false -> Boolean_constant false
				| Boolean_constant true -> 
					(match bc with
					| Boolean_constant false -> Boolean_constant false
					| Boolean_constant true -> Boolean_constant true
					| _ -> bc)
				| _ -> 
					(match bc with
					| Boolean_constant false -> Boolean_constant false
					| Boolean_constant true -> ac
					| _ -> If_then_else(ac,bc,Boolean_constant false)))
			| LOr ->
				(match ac with
				| Boolean_constant true -> Boolean_constant true
				| Boolean_constant false -> 
					(match bc with
					| Boolean_constant false -> Boolean_constant false
					| Boolean_constant true -> Boolean_constant true
					| _ -> bc)
				| _ -> 
					(match bc with
					| Boolean_constant true -> Boolean_constant true
					| Boolean_constant false -> ac
					| _ -> If_then_else(ac,Boolean_constant true,bc)))
			| _ -> Binary(op,ac,bc)
	in
	match prog with
	| Program e -> Program (conditionalTransformHelper e)
	| _ -> prog
	
let deBruijnTransform prog =
	let indices = [] in
	let rec deBruijnTransformHelper indices = function
		| Variable v -> DeBruijn_variable(v,getIndex indices v)
		| DeBruijn_variable(v,c) -> DeBruijn_variable(v,c)
		| Integer_constant i -> Integer_constant i
		| Boolean_constant b -> Boolean_constant b
		| Function_application (f,a) -> Function_application(deBruijnTransformHelper indices f, deBruijnTransformHelper indices a)
		| Anonymous_function (i,d) -> 
			let new_indices = addIdentifier indices i in
			Anonymous_function(i, deBruijnTransformHelper new_indices d)
		| Function(f,p,d,b) -> 
			let new_indices = addIdentifier (addIdentifier indices f) p in
			let body_indices = addIdentifier indices f in
			Function(f,p,deBruijnTransformHelper new_indices d, deBruijnTransformHelper body_indices b)
		| Local_definition(i,d,b) -> 
			let new_indices = addIdentifier indices i in
			Local_definition(i,deBruijnTransformHelper indices d, deBruijnTransformHelper new_indices b)
		| If_then_else (c,t,e) -> If_then_else(deBruijnTransformHelper indices c, deBruijnTransformHelper indices t, deBruijnTransformHelper indices e)
		| Binary(op,a,b) -> Binary(op,deBruijnTransformHelper indices a, deBruijnTransformHelper indices b)
	in 
	match prog with
	| Program e -> Program (deBruijnTransformHelper indices e)
	| _ -> prog
