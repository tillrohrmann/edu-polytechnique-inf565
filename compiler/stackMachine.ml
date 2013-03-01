(* Machine operations *)
type operations =
  | MPlus | MMinus | MMult | MDiv | MLAnd | MLOr | MEquals | MGreater

(* Machine commands *)
and commands =
  | Access of int
  | Apply
  | Cur of commands list
  | Return
  | Let
  | Endlet
  | Branchneg of int
  | Branch of int
  | Op of operations
  | Push of values
  | Rec of commands list
	| Pop
	| CPrint_int
	| CPrint_bool
	| CPair
	| CFirst
	| CSecond

(* values which can be used in the command stream *)
and values =
  | Bool of bool | Int of int

(* value type for the stack and the environment *)
and environment_values =
  | EnvBool of bool
  | EnvInt of int
	| EnvUnit
  | EnvClosure of commands list * environment_values list
	(* Closure for recursive definitions *)
  | EnvRecClosure of commands list * environment_values list
	| EnvPair of environment_values*environment_values

(**
	This function converts a variable of type environment_values into its string
	representation. 
	
	@param 1st of type environment_values
	
	@return string representation of the argument value
*)
let rec string_of_env_value =
  function
  | EnvBool b -> string_of_bool b
  | EnvInt i -> string_of_int i
	| EnvUnit -> "()"
	| EnvPair(a,b) -> "("^(string_of_env_value a)^", "^(string_of_env_value b)^")"
  | _ -> "Function value"

(**
	This function converts a variable of type values into its string representation.
	
	@param 1st of type values
	
	@return string representation of the argument value
*)
let string_of_value =
  function
  | Bool b -> "Bool(" ^ ((string_of_bool b) ^ ")")
  | Int i -> "Int(" ^ ((string_of_int i) ^ ")")

(**
	This function converts the operation of type operations into its string representation. 
	
	@param 1st of type operations
	
	@return string representation of operation
*)
let string_of_operations =
  function
  | MPlus -> "Plus"
  | MMinus -> "Minus"
  | MMult -> "Mult"
  | MDiv -> "Div"
  | MEquals -> "Equals"
  | MGreater -> "Greater"
  | MLAnd -> "LAnd"
  | MLOr -> "LOr"
  
(**
	This function converts a list of machine commands into its string representation. 
	
	@param cmds of type commands list is a list containing the to be converted commands
	
	@return string list of the string representation of the command list
*)
let rec string_of_commands cmds =
  let rec helper =
    function
    | Access i -> "Access " ^ (string_of_int i)
    | Apply -> "Apply"
    | Cur cmds -> "Cur (" ^ ((string_of_commands cmds) ^ ")")
    | Return -> "Return"
    | Let -> "Let"
    | Endlet -> "Endlet"
    | Branchneg i -> "Branchneg " ^ (string_of_int i)
    | Branch i -> "Branch " ^ (string_of_int i)
    | Op op -> "Operation " ^ (string_of_operations op)
    | Push value -> "Push " ^ (string_of_value value)
    | Rec cmds -> "Rec (" ^ ((string_of_commands cmds) ^ ")")
		| Pop -> "Pop"
		| CPrint_int -> "Print_int"
		| CPrint_bool -> "Print_bool"
		| CPair -> "Pair"
		| CFirst -> "First"
		| CSecond -> "Second"
  in
    match cmds with
    | [] -> ""
    | h :: t -> (helper h) ^ (" ; " ^ (string_of_commands t))
 
(**
	This function removes the first number elements from the list li and returns
	the result. 
	
	@param li 'a list 
	@param number specifying the number of elements to be removed from the list
	
	@return 'a list tail of li
*) 
let rec tail li number =
  if number <= 0 then li else tail (List.tl li) (number - 1)

(**
	This function executes a binary machine operation. 
	
	@param (Operation, OperandA, OperandB) operation is of type operations, OperandA is of type
		environment_values and OperandB is of type environment_values. 
		
	@return the result of the operation performed on OperandA and OperandB
*)
let executeOperation =
  function
  | (MPlus, EnvInt a, EnvInt b) -> EnvInt (a + b)
  | (MMinus, EnvInt a, EnvInt b) -> EnvInt (a - b)
  | (MMult, EnvInt a, EnvInt b) -> EnvInt (a * b)
  | (MDiv, EnvInt a, EnvInt b) -> EnvInt (a / b)
  | (MLAnd, EnvBool a, EnvBool b) -> EnvBool (a && b)
  | (MLOr, EnvBool a, EnvBool b) -> EnvBool (a || b)
  | (MEquals, a, b) -> EnvBool (a = b)
  | (MGreater, EnvInt a, EnvInt b) -> EnvBool (a > b)
  | _ -> failwith "Invalid arguments for operator.\n"

(**
	This function takes a list of machine commands and executes them. When the last command
	is executed and the stack contains a value, then this value is printed to stdout. If not,
	then "()" is printed.
	
	@param 1st of commands list. List of machine commands which are to be executed
*)  
let rec executeStackMachine =
  function
  | ([], _, [], _) -> print_string "()\n"
  | ([], _, h :: t, _) -> print_string ((string_of_env_value h) ^ "\n")
  | (Access n :: c, e, s, r) ->
      (Debug.debug
         ("Access " ^
            ((string_of_int n) ^
               (":" ^ (string_of_env_value (List.nth e (n - 1))))));
       executeStackMachine (c, e, ((List.nth e (n - 1)) :: s), r))
	(* non recursive function call *)
  | (Apply :: c, e, EnvClosure (cmds, env) :: v :: s, r) ->
      (Debug.debug "Call function";
       executeStackMachine (cmds, (v :: env), s, ((EnvClosure (c, e)) :: r)))
	(* recursive function call => The recursive function closure has to be *)
	(* placed as well on the environment stack to be callable again. *)
  | (Apply :: c, e, EnvRecClosure (cmds, env) :: v :: s, r) ->
      (Debug.debug "Call rec function";
       executeStackMachine
         (cmds, (v :: (EnvRecClosure (cmds, env)) :: env), s,
          ((EnvClosure (c, e)) :: r)))
	(* Body of a non recursive function *)
  | (Cur body :: c, e, s, r) ->
      (Debug.debug "Install function";
       executeStackMachine (c, e, ((EnvClosure (body, e)) :: s), r))
	(* Body of a recursive function *)
  | (Rec body :: c, e, s, r) ->
      (Debug.debug "Install rec function";
       executeStackMachine (c, e, ((EnvRecClosure (body, e)) :: s), r))
  | (Return :: c, e, s, EnvClosure (cmds, env) :: r) ->
      (Debug.debug "Return"; executeStackMachine (cmds, env, s, r))
  | (Let :: c, e, v :: s, r) ->
      (Debug.debug ("Let:" ^ (string_of_env_value v));
       executeStackMachine (c, (v :: e), s, r))
  | (Endlet :: c, v :: e, s, r) ->
      (Debug.debug "Endlet"; executeStackMachine (c, e, s, r))
  | (Branch n :: c, e, s, r) ->
      (Debug.debug ("Jump:" ^ (string_of_int n));
       executeStackMachine ((tail c n), e, s, r))
  | (Branchneg n :: c, e, EnvBool true :: s, r) ->
      executeStackMachine (c, e, s, r)
  | (Branchneg n :: c, e, EnvBool false :: s, r) ->
      (Debug.debug ("Jump:" ^ (string_of_int n));
       executeStackMachine ((tail c n), e, s, r))
  | (Op op :: c, e, v :: w :: s, r) ->
      (Debug.debug
         ((string_of_operations op) ^
            ("(" ^
               ((string_of_env_value v) ^
                  ("," ^ ((string_of_env_value w) ^ ")")))));
       executeStackMachine (c, e, ((executeOperation (op, w, v)) :: s), r))
  | (Push (Bool b) :: c, e, s, r) ->
      (Debug.debug ("Push " ^ (string_of_bool b));
       executeStackMachine (c, e, ((EnvBool b) :: s), r))
  | (Push (Int i) :: c, e, s, r) ->
      (Debug.debug ("Push " ^ (string_of_int i));
       executeStackMachine (c, e, ((EnvInt i) :: s), r))
	| (Pop ::c, e,h::s,r) -> executeStackMachine(c,e,s,r)
	| (CPrint_int::c,e,EnvInt(i)::s,r) -> print_string ((string_of_int i)^"\n"); executeStackMachine(c,e,EnvUnit::s,r)
	| (CPrint_bool::c,e,EnvBool(true)::s,r) -> print_string "true\n"; executeStackMachine(c,e,EnvUnit::s,r)
	| (CPrint_bool::c,e,EnvBool(false)::s,r) -> print_string "false\n"; executeStackMachine(c,e,EnvUnit::s,r)
	| (CFirst::c,e,EnvPair(a,b)::s,r) -> executeStackMachine(c,e,a::s,r)
	| (CSecond::c,e,EnvPair(a,b)::s,r) -> executeStackMachine(c,e,b::s,r)
	| (CPair::c,e,a::b::s,r) -> executeStackMachine(c,e,EnvPair(a,b)::s,r)
  | _ -> failwith "Invalid stack machine state.\n"
  
let execute cmds = executeStackMachine (cmds, [], [], [])
  

