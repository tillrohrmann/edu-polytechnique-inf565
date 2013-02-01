open Ml_syntax

let rec getIndex indices v = 
	match indices with
	| [] -> (0,false)
	| (id,count,recursive)::t -> if v = id then (count,recursive) else getIndex t v
	
let addIdentifier indices v recursive =
	let rec helper indices result found =
		match indices with
		| [] -> if found = true then result else (v,1,recursive)::result
		| (id,count,r)::t -> 
			if id = v then helper t ((id,1,recursive)::result) true 
			else let new_count = if r = recursive then count+1 else count in
			helper t ((id,new_count,r)::result) found
	in
	helper indices [] false

let conditionalTransform prog =
	let rec conditionalTransformHelper = function
		| Variable v -> Variable v
		| DeBruijn_variable (v,c) -> DeBruijn_variable(v,c)
		| DeBruijn_variable_rec(v,c) -> DeBruijn_variable_rec(v,c)
		| Integer_constant i -> Integer_constant i
		| Boolean_constant b -> Boolean_constant b
		| Function_application (f,a) -> Function_application (conditionalTransformHelper f,conditionalTransformHelper a)
		| Anonymous_function (i,d) -> Anonymous_function (i,conditionalTransformHelper d)
		| Recursive_function (f,p,d,b) -> Recursive_function(f,p,conditionalTransformHelper d, conditionalTransformHelper b)
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

let deBruijnTransform prog =
	let indices = [] in
	let rec deBruijnTransformHelper indices = function
		| Variable v -> let (index,recursive) = getIndex indices v in
			if recursive then DeBruijn_variable_rec(v,index) else DeBruijn_variable(v,index)
		| DeBruijn_variable(v,c) -> DeBruijn_variable(v,c)
		| DeBruijn_variable_rec(v,c) -> DeBruijn_variable_rec(v,c)
		| Integer_constant i -> Integer_constant i
		| Boolean_constant b -> Boolean_constant b
		| Function_application (f,a) -> Function_application(deBruijnTransformHelper indices f, deBruijnTransformHelper indices a)
		| Anonymous_function (i,d) -> 
			let new_indices = addIdentifier indices i false in
			Anonymous_function(i, deBruijnTransformHelper new_indices d)
		| Recursive_function(f,p,d,b) -> 
			let new_indices = addIdentifier (addIdentifier indices f true) p false in
			let body_indices = addIdentifier indices f true in
			Recursive_function(f,p,deBruijnTransformHelper new_indices d, deBruijnTransformHelper body_indices b)
		| Local_definition(i,d,b) -> 
			let new_indices = addIdentifier indices i true in
			Local_definition(i,deBruijnTransformHelper indices d, deBruijnTransformHelper new_indices b)
		| If_then_else (c,t,e) -> If_then_else(deBruijnTransformHelper indices c, deBruijnTransformHelper indices t, deBruijnTransformHelper indices e)
		| Binary(op,a,b) -> Binary(op,deBruijnTransformHelper indices a, deBruijnTransformHelper indices b)
	in 
	match prog with
	| Program e -> Program (deBruijnTransformHelper indices e)
