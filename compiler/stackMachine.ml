type operations =
	| MPlus
	| MMinus
	| MMult
	| MDiv
	| MLAnd
	| MLOr
	| MEquals
	| MGreater

and value =
	| Int of int
	| Bool of bool

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
	| Push of value

and environment_values = 
	| EnvBool of bool
	| EnvInt of int
	| EnvClosure of (commands list)*(environment_values list)

let rec string_of_env_value = function
	| EnvBool b -> string_of_bool b
	| EnvInt i -> string_of_int i
	| _ -> "Closure"

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
		| (Access n::c,e,s,r) -> executeStackMachine (c,e,(List.nth e (n-1))::s,r)
		| (Apply::c,e,EnvClosure(cmds,env)::v::s,r) -> executeStackMachine(cmds,v::env,s,(EnvClosure(c,e))::r)
		| (Cur body::c,e,s,r) -> executeStackMachine(c,e,EnvClosure(body,e)::s,r)
		| (Return::c,e,s,EnvClosure(cmds,env)::r) -> executeStackMachine(cmds,env,s,r)
		| (Let ::c,e,v::s,r) -> executeStackMachine(c,v::e,s,r)
		| (Endlet::c,v::e,s,r) -> executeStackMachine(c,e,s,r)
		| (Branch n::c,e,s,r) -> executeStackMachine(tail c n,e,s,r)
		| (Branchneg n::c,e,EnvBool true::s,r) -> executeStackMachine(c,e,s,r)
		| (Branchneg n::c,e,EnvBool false::s,r) -> executeStackMachine(tail c n,e,s,r)
		| (Op op::c,e,v::w::s,r) -> executeStackMachine(c,e,executeOperation(op,v,w)::s,r)
		| (Push (Int i)::c,e,s,r) -> executeStackMachine(c,e,EnvInt i::s,r)
		| (Push (Bool b)::c,e,s,r) -> executeStackMachine(c,e,EnvBool b::s,r)
		| _ -> failwith "Invalid stack machine state.\n"

let execute cmds =
	executeStackMachine (cmds,[],[],[])