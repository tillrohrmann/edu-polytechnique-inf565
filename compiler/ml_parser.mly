%{

open Ml_syntax

%}

%token LAND LOR PLUS MINUS DIV MULT F IF THEN ELSE SEMICOLON
%token PRINT_INT PRINT_BOOL
%token LBRACKET RBRACKET
%token LET EQUALS IN ARROW FUN REC GREATER
%token TRUE FALSE
%token <int> INTEGER
%token <string> IDENTIFIER
%token EOF

%start program
%type <Ml_syntax.program> program
%type <Ml_syntax.program_exp> constant
%type <Ml_syntax.program_exp> variable
%type <Ml_syntax.program_exp> arithmetic_operation
%type <Ml_syntax.program_exp> logical_operation
%type <Ml_syntax.program_exp> binary_operation


%right FUN LET IF
%left SEMICOLON
%left EQUALS GREATER
%left UMINUS UPLUS
%left LOR
%left LAND
%left PLUS MINUS
%left DIV MULT
%nonassoc TRUE FALSE INTEGER IDENTIFIER LBRACKET RBRACKET
%left APPLY

%%

program:
	| program_expression EOF {Program($1)}
	

program_expression:
	| LBRACKET program_expression RBRACKET {$2}
	| local_definition {$1}
	| recursive_function {$1}
	| anonymous_function {$1}
	| function_application {$1}
	| binary_operation {$1}
	| expression_block {$1}
	| keywords {$1}
	| variable {$1}
	| constant {$1}
	| condition {$1}

keywords:
	| PRINT_INT %prec TRUE { Keyword(Print_int) }
	| PRINT_BOOL %prec TRUE { Keyword(Print_bool) }

expression_block:
	| program_expression SEMICOLON program_expression { Expression_block($1,$3) }
	
condition:
	| IF program_expression THEN program_expression ELSE program_expression %prec IF {If_then_else($2,$4,$6)}
	
local_definition:
	| LET IDENTIFIER EQUALS program_expression IN program_expression %prec LET { Local_definition($2,$4,$6) }
	
recursive_function:
	| LET REC IDENTIFIER IDENTIFIER EQUALS program_expression IN program_expression %prec LET { Recursive_function($3,$4,$6,$8) }
	
anonymous_function:
	| FUN IDENTIFIER ARROW program_expression %prec FUN { Anonymous_function($2,$4) }
	
function_application:
	| program_expression program_expression %prec APPLY {Function_application($1,$2)}

binary_operation:
	| logical_operation {$1}
	| arithmetic_operation {$1}
	
logical_operation:
	| program_expression LAND program_expression { Binary(LAnd,$1,$3) }
	| program_expression LOR program_expression { Binary(LOr,$1,$3) } 
	| program_expression EQUALS program_expression {Binary(Equals,$1,$3)}
	| program_expression GREATER program_expression {Binary(Greater,$1,$3)}
	
arithmetic_operation:
	| program_expression PLUS program_expression { Binary(Plus,$1,$3) }
	| program_expression MINUS program_expression { Binary(Minus,$1,$3) }
	| program_expression MULT program_expression { Binary(Mult,$1,$3) }
	| program_expression DIV program_expression {Binary(Div,$1,$3) }
	
variable:
	IDENTIFIER {Variable $1}

constant:
	| boolean_constant {$1}
	| integer_constant {$1}
	
boolean_constant:
	| TRUE {Boolean_constant true}
	| FALSE {Boolean_constant false}

integer_constant:
	| INTEGER {Integer_constant $1}
	| LBRACKET MINUS INTEGER RBRACKET {Integer_constant (-$3)}
	
	



