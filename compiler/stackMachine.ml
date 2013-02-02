type operations =
	| MPlus
	| MMinus
	| MMult
	| MDiv
	| MLAnd
	| MLOr
	| MEquals
	| MGreater

and commands = 
	| Access of int
	| Apply
	| Cur of (commands list)
	| Return
	| Let
	| Endlet
	| Branchneg of int
	| Branch of int
	| Op of operations
	| Push of values
	| Rec of (commands list)

and values =
	| Bool of bool
	| Int of int

and environment_values = 
	| EnvBool of bool
	| EnvInt of int
	| EnvClosure of (commands list)*(environment_values list)
	| EnvRecClosure of (commands list)*(environment_values list)

let rec string_of_env_value = function
	| EnvBool b -> string_of_bool b
	| EnvInt i -> string_of_int i
	| _ -> "Closure"

let string_of_value = function
	| Bool b -> "Bool("^(string_of_bool b)^")"
	| Int i -> "Int("^(string_of_int i)^")"

let string_of_operations = function
	| MPlus -> "Plus"
	| MMinus -> "Minus"
	| MMult -> "Mult"
	| MDiv -> "Div"
	| MEquals -> "Equals"
	| MGreater -> "Greater"
	| MLAnd -> "LAnd"
	| MLOr -> "LOr"

let rec string_of_commands cmds =
	let rec helper = function
	| Access i -> "Access "^(string_of_int i)
	| Apply -> "Apply"
	| Cur cmds -> "Cur ("^(string_of_commands cmds)^")"
	| Return -> "Return"
	| Let -> "Let"
	| Endlet -> "Endlet"
	| Branchneg i -> "Branchneg "^(string_of_int i)
	| Branch i -> "Branch "^(string_of_int i)
	| Op op -> "Operation "^(string_of_operations op)
	| Push value -> "Push "^(string_of_value value)
	| Rec cmds -> "Rec ("^(string_of_commands cmds)^")"
	in
	match cmds with
		| [] -> ""
		| h::t -> (helper h)^" ; "^(string_of_commands t)

let rec tail li number =
	if number <= 0 then
		li
	else
		tail (List.tl li) (number-1)
		
let executeOperation = function
	| (MPlus,EnvInt a, EnvInt b) -> EnvInt(a+b)
	| (MMinus,EnvInt a, EnvInt b) -> EnvInt(a-b)
	| (MMult,EnvInt a, EnvInt b) -> EnvInt(a*b)
	| (MDiv,EnvInt a, EnvInt b) -> EnvInt(a/b)
	| (MLAnd, EnvBool a, EnvBool b) -> EnvBool(a && b)
	| (MLOr, EnvBool a, EnvBool b) -> EnvBool (a || b)
	| (MEquals, a, b) -> EnvBool (a = b)
	| (MGreater,EnvInt a,EnvInt b) -> EnvBool (a > b)
	| _ -> failwith "Invalid arguments for operator.\n"

let rec executeStackMachine = function
		| ([],_,[],_) -> print_string "()\n"
		| ([],_,h::t,_) -> print_string ((string_of_env_value h)^"\n")
		| (Access n::c,e,s,r) -> 
			Debug.debug("Access "^(string_of_int n)^":"^(string_of_env_value (List.nth e (n-1))));
			executeStackMachine (c,e,(List.nth e (n-1))::s,r)
		| (Apply::c,e,EnvClosure(cmds,env)::v::s,r) ->
			Debug.debug("Call function");
			executeStackMachine(cmds,v::env,s,(EnvClosure(c,e))::r)
		| (Apply::c,e,EnvRecClosure(cmds,env)::v::s,r) ->
			Debug.debug("Call rec function"); 
			executeStackMachine(cmds,v::EnvRecClosure(cmds,env)::env,s,(EnvClosure(c,e))::r)
		| (Cur body::c,e,s,r) -> 
			Debug.debug("Install function");
			executeStackMachine(c,e,EnvClosure(body,e)::s,r)
		| (Rec body::c,e,s,r) -> 
			Debug.debug("Install rec function");
			executeStackMachine(c,e,EnvRecClosure(body,e)::s,r)
		| (Return::c,e,s,EnvClosure(cmds,env)::r) -> 
			Debug.debug("Return");
			executeStackMachine(cmds,env,s,r)
		| (Let ::c,e,v::s,r) -> 
			Debug.debug("Let:"^(string_of_env_value v));
			executeStackMachine(c,v::e,s,r)
		| (Endlet::c,v::e,s,r) -> 
			Debug.debug("Endlet");
			executeStackMachine(c,e,s,r)
		| (Branch n::c,e,s,r) -> 
			Debug.debug("Jump:"^(string_of_int n));
			executeStackMachine(tail c n,e,s,r)
		| (Branchneg n::c,e,EnvBool true::s,r) -> executeStackMachine(c,e,s,r)
		| (Branchneg n::c,e,EnvBool false::s,r) -> 
			Debug.debug("Jump:"^(string_of_int n));
			executeStackMachine(tail c n,e,s,r)
		| (Op op::c,e,v::w::s,r) -> 
			Debug.debug(string_of_operations op^"("^(string_of_env_value v)^","^(string_of_env_value w)^")");
			executeStackMachine(c,e,executeOperation(op,w,v)::s,r)
		| (Push (Bool b)::c,e,s,r) ->
			Debug.debug("Push "^(string_of_bool b) );
			executeStackMachine(c,e,EnvBool b::s,r)
		| (Push (Int i)::c,e,s,r) -> 
			Debug.debug("Push "^(string_of_int i));
			executeStackMachine(c,e,EnvInt i::s,r)
		| _ -> failwith "Invalid stack machine state.\n"

let execute cmds =
	executeStackMachine (cmds,[],[],[])