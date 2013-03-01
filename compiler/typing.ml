open Ml_syntax
  
open Types

(* global counter for the type variables => each of them is unique *)  
let counter = ref 0

(**
	This function creates a new unique type variable and stores the mapping
	type variable -> type in the given hash map. Initially that is the identity. 
	
	@param mapping hash map which stores the mapping (type variable -> type)
	
	@return type variable of type Types.ml_types
*)
let getNewTypeVariable mapping =
  let result = Type_variable !counter
  in
    (Hashtbl.replace !mapping !counter result;
     counter := !counter + 1;
     result)

(**
	This function checks whether the to parameters are equal. 
*)
let identicalTypes a b = a = b

(**
	This function checks whether the type variable indicated by a is contained in the
	type expression b. 
	
	@param a type expression of type Types.ml_types
	@param b type expression of type Types.ml_types 
	
	@return bool of the result
*)
let rec containedIn a b =
  match b with
  | Function (c, d) ->
      (identicalTypes a c) ||
        ((identicalTypes a d) || ((containedIn a c) || (containedIn a d)))
	| PairType(c,d) -> (identicalTypes a c) || (identicalTypes a d) || (containedIn a c) || (containedIn a d)
  | _ -> false

(**
	This function computes the union of the two types indicated by parameters a and b. 
	If one of the parameters is a type variable then it is verified that no type variable
	is contained in another type expression. If this is nevertheless the case, then the
	two types can not be combined and None is returned. If the type variable is not contained
	in the other type expression, then the result is the other type. If none of the type expressions
	is a type variable and the types coincide then the identical type is returned. In all other 
	cases None is returned. 
	
	@param a type expression of type Types.ml_types
	@param b type expression of type Types.ml_types
	
	@return If types are not compatible then None, else Some( union result )
*)
let rec union a b =
  if identicalTypes a b
  then Some a
  else
    (match a with
     | Type_variable c -> if (containedIn a b) = false then Some b else None
     | _ ->
         (match b with
          | Type_variable c ->
              if (containedIn b a) = false then Some a else None
          | _ -> None))

(**
	This function retrieves for a type expression the current type. For basic
	types this is straightforward but for type variables which have already been
	unified one has to look up the current type in the type variable mapping. Since
	several type variables might be chained, one refreshes the mapping of the requested
	type variable at the end with the final result in order to make future request more
	efficient.
	
	@param mapping hash map containing the mapping between type variable and actual type
	@param a type expression for which the type shall be retrieved
	
	@return the current type of the type expression
*) 
let getType mapping a =
  let rec helper old cur =
    match cur with
    | Type_variable i ->
        let t = Hashtbl.find !mapping i
        in
					(* if the current mapping of i equals the old type, then we have *)
					(* reached a fix point and can return the type. *)
          if t = old
          then old
					(* if not, then we have found a new type variable and have to retrieve*)
					(* the type for that variable. *)
          else
            (let result = helper cur t
             in 
						(* set the mapping to the final value to make future request more*)
						(* efficient *)
						(Hashtbl.replace !mapping i result; result))
    | Function (p, b) -> Function ((helper p p), (helper b b))
		| PairType (l,r) -> PairType((helper l l),(helper r r))
    | _ -> cur
  in helper a a

(**
	This function sets the type mapping for a type variable. 
	
	@param mapping hash map containing the type variable, type mapping
	@param a type expression for which the new type has to be set. It
		has only an effect if it is a type variable
	@param b type expression which is the new type of a if it is a type
		variable
*)
let updateMapping mapping a b =
  match a with | Type_variable i -> Hashtbl.replace !mapping i b | _ -> ()
  
(**
	This function performs the type unification algorithm on two type expressions
	a and b. If both types represent a functional type, then the parameter type
	and the definition type of both types have to be unified. The same holds for pairs,
	just that the respective first and second elements of both pairs are unified.
  If not, then the unification is achieved by simply calling the union algorithm. 
	If the types are unifiable, then the mapping of possible type variables is set to this 
	new unified type. 
	
	@param mapping hash map containing the type variable, type mapping
	@param a type expression which shall be unified with b
	@param b type expression which shall be unified with a
	
	@return if types are unifiable, then return Some( unified type ), else return None
*)
let rec unify mapping a b =
  let a_type = getType mapping a in
  let b_type = getType mapping b in
  let result =
    match a_type with
    | Function (c, d) ->
        (match b_type with
         | Function (e, f) ->
             (match unify mapping c e with
              | Some t ->
                  (match unify mapping d f with
                   | Some tb -> Some (Function (t, tb))
                   | None -> None)
              | None -> None)
         | _ -> union a_type b_type)
		| PairType (c,d) ->
			(match b_type with
				| PairType(e,f) ->
					(match unify mapping c e with
						| Some t ->
							(match unify mapping d f with
								| Some tb -> Some(PairType(t,tb))
								| None -> None)
						| None -> None)
				| _ -> union a_type b_type)
    | _ -> union a_type b_type
  in
    ((match result with
      | Some t ->
				(* update the type variable, type mapping with the new type *)
          (updateMapping mapping a_type t; updateMapping mapping b_type t)
      | None -> ());
     result)

(**
	This function converts a variable of type Types.ml_types into its string representation.
	
	@param 1st variable of type Types.ml_types which is supposed to be converted. 
	
	@return string representation of the given argument
*)
let rec string_of_ml_types =
  function
  | Int -> "int"
  | Bool -> "bool"
  | Function (p, b) ->
      (string_of_ml_types p) ^ (" -> " ^ (string_of_ml_types b))
	| PairType(a,b) -> (string_of_ml_types a) ^ ("*") ^ (string_of_ml_types b)
  | Type_variable c -> "'" ^ (string_of_int c)
  | Universal t -> string_of_ml_types t
	| Unit -> "unit"

(**
	This function calculates for a given binary operation the type.
	
	@param mapping hash map containing the type variable, type mapping
	@param op of type StackMachine.operations for which the type shall be computed
	
	@return type of type Types.ml_types
*) 
let operation_type mapping op =
  match op with
  | Plus -> Function (Int, (Function (Int, Int)))
  | Minus -> Function (Int, (Function (Int, Int)))
  | Mult -> Function (Int, (Function (Int, Int)))
  | Div -> Function (Int, (Function (Int, Int)))
  | LAnd -> Function (Bool, (Function (Bool, Bool)))
  | LOr -> Function (Bool, (Function (Bool, Bool)))
  | Greater -> Function (Int, (Function (Int, Bool)))
  | Equals ->
      let typeVar = getNewTypeVariable mapping
      in Function (typeVar, (Function (typeVar, Bool)))

(**
	This function checks whether the list env contains an element t. 
	
	@param env of type 'a list and for which shall be checked whether it contains t or not
	@param t element which shall be searched in env
	
	@return true iff t is in env, false iff t is not in env
*)
let rec envContainsType env t =
  match env with
  | [] -> false
  | h :: tail -> if h = t then true else envContainsType tail t
  
(**
	This function takes a type expression and generalizes all its type variables
	which are not contained in env and env_rec by fresh new type variables.
	
	@param env Types.ml_types list which contains type variables which shall not be replaced
	@param mapping hash map containing the type variable, type mapping
	@param t type expression which shall be generalized
	
	@return generalized type (=> all free type variables have been replaced by fresh new type
		variables)
*)
let generalizeType env mapping t =
	(* this hash map is used to keep track of the replaced type variables,*)
	(* assuring the same type variable is always replaced by the same new type variable *)
  let typeVarMapping = Hashtbl.create 20 in
  let rec helper =
    function
    | Universal t -> helper t
    | Function (p, b) -> Function ((helper p), (helper b))
		| PairType(l,r) -> PairType((helper l), (helper r))
    | Type_variable i ->
        if
          (envContainsType env (Type_variable i))
        then Type_variable i
        else
          (try Hashtbl.find typeVarMapping i
           with
           | _ ->
               let newTypeVar = getNewTypeVariable mapping
               in (Hashtbl.replace typeVarMapping i newTypeVar; newTypeVar))
    | (_ as x) -> x
  in helper t
  
(**
	This function calculates for a given program expression the resulting type.
	For that purpose it uses the Hindley-Milner algorithm to infer the missing
	types. The types for variables and parameters are stored in list of types
	such that the De Bruijn index of a variable is also the index of the array
	where this type has been stored.
	
	@param env Types.ml_types list containing the types for the De Bruijn indices
	@param env_rec Types.ml_types list obsolete
	@param mapping hash map containing the type variable, type mapping 
	@param exp program expression for which the type shall be computed
	
	@return the resulting type of type Types.ml_types
*)
let rec expression_type env mapping exp =
  match exp with
  | Boolean_constant _ -> Bool
  | Integer_constant _ -> Int
  | Variable v ->
      failwith "Program has to be transformed to use De Bruijn indices\n"
  | DeBruijn_variable (v, i) ->
			(* Get type from environment *)
      (match getType mapping (List.nth env (i - 1)) with
			(* If the type has a universal quantifier, then generalize the type to obtain a*)
			(* concrete type *)
       | Universal t -> generalizeType env mapping t
       | (_ as r) -> r)
  | Anonymous_function (p, d) ->
			(* introduce new type variable for the yet to be calculated parameter type *)
      let newTypeVar = getNewTypeVariable mapping in
      let result = expression_type (newTypeVar :: env) mapping d
      in 
			(* resolve the type variable of the parameter to obtain the most precise type *)
			Function ((getType mapping newTypeVar), result)
  | Recursive_function (f, p, d, b) ->
			(* introduce new type variables for the function itself and its parameters *)
      let funType = getNewTypeVariable mapping in
      let parType = getNewTypeVariable mapping in
			(* calculate the type of the function *)
      let resultFunType =
        expression_type (parType :: funType :: env) mapping d
      in
			(* Universally quantify the newly computed function type *)
        expression_type
          ((Universal (Function ((getType mapping parType), resultFunType))) ::
            env) mapping b
  | Function_application (f, a) ->
			(* calculate the function type *)
      let funType = expression_type env mapping f in
			(* calculate the argument type *)
      let argumentType = expression_type env mapping a in
      let newTypeVar = getNewTypeVariable mapping in
      let result =
				(* unify the function type with Function(argumentType, newTypeVar) *)
				(* to obtain the actual type of the function application. *)
        unify mapping funType (Function (argumentType, newTypeVar))
      in
        (match result with
         | Some t -> 
					(* get mapping of type variable in case that is has be instantiated *)
					getType mapping newTypeVar
				(* if the unification failed, then there is an typing error *)
         | None ->
             (match funType with
              | Function (p, f) ->
                  failwith
                    ("The argument has type " ^
                       ((string_of_ml_types argumentType) ^
                          (" but an expression of type " ^
                             ((string_of_ml_types p) ^ " was expected"))))
              | _ ->
                  failwith
                    ("Could not unify type " ^
                       ((string_of_ml_types funType) ^
                          (" and " ^
                             (string_of_ml_types
                                (Function (argumentType, newTypeVar))))))))
  | Binary (op, a, b) ->
      let op_type = operation_type mapping op in
      let a_type = expression_type env mapping a in
      let b_type = expression_type env mapping b
      in
        (match op_type with
         | Function (p, b) ->
						(* try to unify first argument with first parameter *)
             (match unify mapping p a_type with
              | Some ta ->
                  (match b with
                   | Function (p, b) ->
											(* try to unify second argument with second parameter *)
                       (match unify mapping p b_type with
                        | Some tb -> b
                        | None ->
                            failwith
                              ("The argument has type " ^
                                 ((string_of_ml_types b_type) ^
                                    (" but an expression of type " ^
                                       ((string_of_ml_types p) ^
                                          " was expected.")))))
                   | _ ->
                       failwith
                         ("The function of type " ^
                            ((string_of_ml_types op_type) ^
                               " is applied to too many arguments.")))
              | None ->
                  failwith
                    ("The argument has type " ^
                       ((string_of_ml_types a_type) ^
                          (" but an expression of type " ^
                             ((string_of_ml_types p) ^ " was expected.")))))
         | _ -> failwith "The expression is not a function.")
  | Local_definition (v, d, b) ->
      let v_type = expression_type env mapping d
      in expression_type ((Universal v_type) :: env) mapping b
	| Expression_block(e1,e2) ->
			(* calculate e1, because there might be type variables which *)
			(* are set within this expression. *)
			ignore (expression_type env mapping e1);
			expression_type env mapping e2
	| Keyword(Print_int) -> Function(Int,Unit)
	| Keyword(Print_bool) -> Function(Bool,Unit)
	| Keyword(First) -> 
		let typeVarA = getNewTypeVariable mapping in
		let typeVarB = getNewTypeVariable mapping in
		Function(PairType(typeVarA,typeVarB),typeVarA)
	| Keyword(Second) -> 
		let typeVarA = getNewTypeVariable mapping in
		let typeVarB = getNewTypeVariable mapping in
		Function(PairType(typeVarA,typeVarB),typeVarB)
	| Pair(a,b) -> PairType(expression_type env mapping a, expression_type env mapping b)
  | If_then_else (c, t, e) ->
      let condition_type = expression_type env mapping c
      in
				(* condition type has to be bool *)
        if ( != ) condition_type Bool
        then
          failwith
            ("Condition has type " ^
               ((string_of_ml_types condition_type) ^
                  (" but an expression of type " ^
                     ((string_of_ml_types Bool) ^ " was expected."))))
        else
          (let then_type = expression_type env mapping t in
           let else_type = expression_type env mapping e
           in
						(* then type und else type have to unifiable to a common type *)
             match unify mapping then_type else_type with
             | Some t -> t
             | None ->
                 failwith
                   ("Else expression has type " ^
                      ((string_of_ml_types else_type) ^
                         (" but an expression of type " ^
                            ((string_of_ml_types then_type) ^
                               " was expected.")))))
  
let program_type prog =
  let mapping = ref (Hashtbl.create 20)
  in match prog with | Program exp -> expression_type [] mapping exp
  

