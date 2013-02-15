open Ml_syntax
 
(**
	This function retrieves from a list of type (string*int) the second value
	of a pair (x,y) with x=v where v is a parameter of the function. If such
	a pair does not exist, then 0 will be returned. 
	
	@param indices (string*int) list which stores the current De Bruijn indices
		for the specific scope. 
	@param v name of variable for which the De Bruijn index shall be retrieved.
	
	@return De Bruijn index of variable v or 0 if the variable is unbound
*)
let rec getIndex indices v =
  match indices with
  | [] -> 0
  | (id, count) :: t -> if v = id then count else getIndex t v
 
(**
	This function has the task to keep track of the current De Bruijn indices for
	a set of variables. For this purpose the De Bruijn index assignments will be
	stored in a list of type (string*int) list. Every time a new variable binding
	is encountered this function with the current list and the new variable name
	will be called to adjust the De Bruijn indices. The De Bruijn index of every
	variable will be increased by 1 except for the newly introduced variable which
	gets an index of 1. 
	
	@param indices is a (string*int) list representing the current De Bruijn index
		assignment.
	@param v is the name of the newly introduced variable.
	
	@return update list of name-indices mapping
*)
let addIdentifier indices v =
  let rec helper indices result found =
    match indices with
    | [] -> if found = true then result else (v, 1) :: result
    | (id, count) :: t ->
        if id = v
        then helper t ((id, 1) :: result) true
        else helper t ((id, (count + 1)) :: result) found
  in helper indices [] false
  
(**
	This function performs the following AST transformation. All logical operations will
	be simplified as far as possible (e.g. x && TRUE => x) and then represented by an
	if_then_else expression:
	
	And: x and y => if x then y else false
	Or: x or y => if x then true else y
	
	This assures the short circuit evaluation of the expressions.
	
	@param prog of type program representing the AST of a parsed program. 
	
	@result the transformed AST.
*)
let conditionalTransform prog =
  let rec conditionalTransformHelper =
    function
    | Variable v -> Variable v
    | DeBruijn_variable (v, c) -> DeBruijn_variable (v, c)
    | Integer_constant i -> Integer_constant i
    | Boolean_constant b -> Boolean_constant b
    | Function_application (f, a) ->
        Function_application ((conditionalTransformHelper f),
          (conditionalTransformHelper a))
    | Anonymous_function (i, d) ->
        Anonymous_function (i, (conditionalTransformHelper d))
    | Recursive_function (f, p, d, b) ->
        Recursive_function (f, p, (conditionalTransformHelper d),
          (conditionalTransformHelper b))
    | Local_definition (i, d, b) ->
        Local_definition (i, (conditionalTransformHelper d),
          (conditionalTransformHelper b))
    | If_then_else (c, t, e) ->
        If_then_else ((conditionalTransformHelper c),
          (conditionalTransformHelper t), (conditionalTransformHelper e))
		| Expression_block (e1, e2) ->
				Expression_block(conditionalTransformHelper e1,conditionalTransformHelper e2)
		| Keyword(_) as keyword -> keyword
    | Binary (op, a, b) ->
        let ac = conditionalTransformHelper a in
        let bc = conditionalTransformHelper b
        in
          (match op with
           | LAnd ->
               (match ac with
								(* Simplifications if the left side is evaluated to a boolean constant *)
                | Boolean_constant false -> Boolean_constant false
                | Boolean_constant true ->
                    (match bc with
                     | Boolean_constant false -> Boolean_constant false
                     | Boolean_constant true -> Boolean_constant true
                     | _ -> bc)
                | _ ->
                    (match bc with
										(* Simplifications if the right side is evaluated to a boolean constant *)
                     | Boolean_constant false -> Boolean_constant false
                     | Boolean_constant true -> ac
                     | _ -> If_then_else (ac, bc, (Boolean_constant false))))
           | LOr ->
               (match ac with
								(* Simplifications if the left side is evaluated to a boolean constant *)
                | Boolean_constant true -> Boolean_constant true
                | Boolean_constant false ->
                    (match bc with
                     | Boolean_constant false -> Boolean_constant false
                     | Boolean_constant true -> Boolean_constant true
                     | _ -> bc)
                | _ ->
                    (match bc with
										(* Simplifications if the right side is evaluated to a boolean constant *)
                     | Boolean_constant true -> Boolean_constant true
                     | Boolean_constant false -> ac
                     | _ -> If_then_else (ac, (Boolean_constant true), bc)))
           | _ -> Binary (op, ac, bc))
  in match prog with | Program e -> Program (conditionalTransformHelper e)
  
(**
	This function performs the following transformation on the given program:
	All variable accesses are substituted by its corresponding De Bruijn index. This
	simplifies later the retrieval of the variable on the stack because the De Bruijn
	index is the its index on the stack. 
	
	@param prog program of type program which shall be transformed.
	
	@return transformed AST
*)
let deBruijnTransform prog =
  let indices = [] in
  let rec deBruijnTransformHelper indices =
    function
    | Variable v ->
				(* replace v by its De Bruijn index *)
        let index = getIndex indices v in DeBruijn_variable (v, index)
    | DeBruijn_variable (v, c) -> DeBruijn_variable (v, c)
    | Integer_constant i -> Integer_constant i
    | Boolean_constant b -> Boolean_constant b
    | Function_application (f, a) ->
        Function_application ((deBruijnTransformHelper indices f),
          (deBruijnTransformHelper indices a))
    | Anonymous_function (i, d) ->
				(* introduce newly binded variable *)
        let new_indices = addIdentifier indices i
        in Anonymous_function (i, (deBruijnTransformHelper new_indices d))
    | Recursive_function (f, p, d, b) ->
				(* introduce newly binded variables *)
        let new_indices = addIdentifier (addIdentifier indices f) p in
        let body_indices = addIdentifier indices f
        in
          Recursive_function (f, p, (deBruijnTransformHelper new_indices d),
            (deBruijnTransformHelper body_indices b))
    | Local_definition (i, d, b) ->
				(* introduce newly binded variables *)
        let new_indices = addIdentifier indices i
        in
          Local_definition (i, (deBruijnTransformHelper indices d),
            (deBruijnTransformHelper new_indices b))
		| Expression_block (e1, e2) ->
				Expression_block(deBruijnTransformHelper indices e1, deBruijnTransformHelper indices e2)
		| Keyword(_) as keyword -> keyword
    | If_then_else (c, t, e) ->
        If_then_else ((deBruijnTransformHelper indices c),
          (deBruijnTransformHelper indices t),
          (deBruijnTransformHelper indices e))
    | Binary (op, a, b) ->
        Binary (op, (deBruijnTransformHelper indices a),
          (deBruijnTransformHelper indices b))
  in
    match prog with
    | Program e -> Program (deBruijnTransformHelper indices e)
  

