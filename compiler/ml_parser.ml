type token =
  | LAND
  | LOR
  | PLUS
  | MINUS
  | DIV
  | MULT
  | F
  | IF
  | THEN
  | ELSE
  | SEMICOLON
  | COMMA
  | PRINT_INT
  | PRINT_BOOL
  | FIRST
  | SECOND
  | LBRACKET
  | RBRACKET
  | LET
  | EQUALS
  | IN
  | ARROW
  | FUN
  | REC
  | GREATER
  | TRUE
  | FALSE
  | INTEGER of (int)
  | IDENTIFIER of (string)
  | EOF

open Parsing;;
# 2 "ml_parser.mly"

open Ml_syntax

# 39 "ml_parser.ml"
let yytransl_const = [|
  257 (* LAND *);
  258 (* LOR *);
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* DIV *);
  262 (* MULT *);
  263 (* F *);
  264 (* IF *);
  265 (* THEN *);
  266 (* ELSE *);
  267 (* SEMICOLON *);
  268 (* COMMA *);
  269 (* PRINT_INT *);
  270 (* PRINT_BOOL *);
  271 (* FIRST *);
  272 (* SECOND *);
  273 (* LBRACKET *);
  274 (* RBRACKET *);
  275 (* LET *);
  276 (* EQUALS *);
  277 (* IN *);
  278 (* ARROW *);
  279 (* FUN *);
  280 (* REC *);
  281 (* GREATER *);
  282 (* TRUE *);
  283 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTEGER *);
  285 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\015\000\013\000\013\000\
\013\000\013\000\012\000\014\000\008\000\009\000\010\000\011\000\
\006\000\006\000\005\000\005\000\005\000\005\000\004\000\004\000\
\004\000\004\000\003\000\002\000\002\000\016\000\016\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\005\000\001\000\001\000\
\001\000\001\000\003\000\006\000\006\000\008\000\004\000\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\016\000\017\000\018\000\000\000\
\000\000\000\000\038\000\039\000\040\000\035\000\042\000\010\000\
\009\000\026\000\025\000\006\000\000\000\002\000\003\000\004\000\
\005\000\007\000\008\000\011\000\012\000\036\000\037\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\015\000\016\000\017\000\018\000\019\000\020\000\048\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000"

let yysindex = "\002\000\
\163\001\000\000\163\001\000\000\000\000\000\000\000\000\127\001\
\246\254\231\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\110\255\
\233\254\077\255\252\254\242\254\004\255\163\001\163\001\163\001\
\163\001\163\001\163\001\163\001\163\001\163\001\000\000\002\255\
\163\001\010\255\163\001\000\000\000\255\163\001\163\001\110\001\
\083\001\146\001\146\001\180\001\180\001\007\255\077\001\077\001\
\147\255\000\000\185\255\011\255\215\255\048\001\163\001\000\000\
\163\001\163\001\048\001\019\001\048\001\163\001\048\001"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\159\000\
\179\000\109\000\134\000\059\000\084\000\233\000\197\000\215\000\
\000\000\000\000\000\000\000\000\000\000\154\000\000\000\000\000\
\000\000\000\000\237\000\000\000\250\000\000\000\254\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yytablesize = 721
let yytable = "\021\000\
\024\000\032\000\001\000\037\000\050\000\054\000\034\000\038\000\
\039\000\040\000\041\000\042\000\043\000\035\000\004\000\005\000\
\006\000\007\000\036\000\004\000\005\000\006\000\007\000\008\000\
\053\000\055\000\045\000\066\000\068\000\047\000\073\000\046\000\
\011\000\012\000\013\000\014\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\000\000\000\000\065\000\
\000\000\067\000\000\000\000\000\069\000\070\000\000\000\000\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\075\000\000\000\076\000\
\077\000\000\000\000\000\000\000\079\000\038\000\039\000\040\000\
\041\000\042\000\043\000\033\000\003\000\000\000\000\000\044\000\
\051\000\004\000\005\000\006\000\007\000\008\000\052\000\009\000\
\045\000\000\000\000\000\010\000\000\000\046\000\011\000\012\000\
\013\000\014\000\000\000\000\000\031\000\000\000\038\000\039\000\
\040\000\041\000\042\000\043\000\000\000\003\000\049\000\000\000\
\044\000\000\000\004\000\005\000\006\000\007\000\008\000\000\000\
\009\000\045\000\000\000\000\000\010\000\032\000\046\000\011\000\
\012\000\013\000\014\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\039\000\040\000\041\000\042\000\
\043\000\023\000\003\000\000\000\071\000\044\000\027\000\004\000\
\005\000\006\000\007\000\008\000\000\000\009\000\045\000\000\000\
\000\000\010\000\000\000\046\000\011\000\012\000\013\000\014\000\
\000\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\039\000\040\000\041\000\042\000\043\000\000\000\
\003\000\000\000\000\000\044\000\029\000\004\000\005\000\006\000\
\007\000\008\000\072\000\009\000\045\000\000\000\000\000\010\000\
\000\000\046\000\011\000\012\000\013\000\014\000\030\000\038\000\
\039\000\040\000\041\000\042\000\043\000\000\000\003\000\000\000\
\000\000\044\000\000\000\004\000\005\000\006\000\007\000\008\000\
\019\000\009\000\045\000\074\000\020\000\010\000\000\000\046\000\
\011\000\012\000\013\000\014\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\022\000\000\000\000\000\
\000\000\024\000\024\000\024\000\024\000\024\000\024\000\000\000\
\024\000\024\000\024\000\024\000\024\000\000\000\000\000\000\000\
\000\000\024\000\024\000\024\000\024\000\024\000\000\000\024\000\
\000\000\024\000\024\000\024\000\024\000\024\000\038\000\039\000\
\040\000\041\000\042\000\043\000\000\000\003\000\000\000\000\000\
\044\000\000\000\004\000\005\000\006\000\007\000\008\000\000\000\
\009\000\045\000\000\000\000\000\010\000\000\000\046\000\011\000\
\012\000\013\000\014\000\034\000\034\000\034\000\034\000\034\000\
\034\000\000\000\034\000\034\000\034\000\034\000\034\000\000\000\
\000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
\000\000\034\000\000\000\034\000\033\000\033\000\033\000\033\000\
\033\000\033\000\000\000\033\000\033\000\033\000\033\000\033\000\
\000\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\000\000\033\000\000\000\033\000\031\000\031\000\031\000\
\031\000\000\000\000\000\000\000\031\000\031\000\031\000\031\000\
\031\000\000\000\000\000\000\000\000\000\000\000\031\000\031\000\
\031\000\031\000\000\000\031\000\000\000\031\000\032\000\032\000\
\032\000\032\000\000\000\000\000\000\000\032\000\032\000\032\000\
\032\000\032\000\000\000\000\000\000\000\000\000\000\000\032\000\
\032\000\032\000\032\000\000\000\032\000\000\000\032\000\027\000\
\027\000\000\000\023\000\023\000\000\000\023\000\027\000\027\000\
\027\000\027\000\027\000\023\000\000\000\000\000\023\000\000\000\
\027\000\027\000\027\000\027\000\028\000\027\000\000\000\027\000\
\000\000\000\000\028\000\028\000\028\000\028\000\028\000\000\000\
\000\000\000\000\000\000\000\000\028\000\028\000\028\000\028\000\
\000\000\028\000\000\000\028\000\029\000\029\000\029\000\029\000\
\029\000\000\000\000\000\000\000\000\000\000\000\029\000\029\000\
\029\000\029\000\000\000\029\000\000\000\029\000\030\000\030\000\
\030\000\030\000\030\000\000\000\000\000\000\000\000\000\000\000\
\030\000\030\000\030\000\030\000\000\000\030\000\000\000\030\000\
\019\000\019\000\019\000\019\000\019\000\020\000\020\000\000\000\
\020\000\000\000\019\000\019\000\000\000\019\000\020\000\019\000\
\000\000\020\000\021\000\021\000\000\000\021\000\022\000\022\000\
\000\000\022\000\000\000\021\000\000\000\000\000\021\000\022\000\
\000\000\000\000\022\000\038\000\039\000\040\000\041\000\042\000\
\043\000\000\000\003\000\000\000\000\000\044\000\000\000\004\000\
\005\000\006\000\007\000\008\000\000\000\009\000\045\000\078\000\
\000\000\010\000\000\000\046\000\011\000\012\000\013\000\014\000\
\038\000\039\000\040\000\041\000\042\000\043\000\000\000\003\000\
\000\000\000\000\044\000\000\000\004\000\005\000\006\000\007\000\
\008\000\000\000\009\000\045\000\000\000\000\000\010\000\000\000\
\046\000\011\000\012\000\013\000\014\000\038\000\039\000\040\000\
\041\000\042\000\043\000\038\000\000\000\040\000\041\000\042\000\
\043\000\004\000\005\000\006\000\007\000\008\000\000\000\004\000\
\005\000\006\000\007\000\008\000\000\000\000\000\011\000\012\000\
\013\000\014\000\000\000\000\000\011\000\012\000\013\000\014\000\
\040\000\041\000\042\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\005\000\006\000\007\000\008\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\003\000\011\000\
\012\000\013\000\014\000\004\000\005\000\006\000\007\000\008\000\
\000\000\009\000\000\000\000\000\000\000\010\000\042\000\043\000\
\011\000\012\000\013\000\014\000\000\000\000\000\004\000\005\000\
\006\000\007\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\011\000\012\000\013\000\014\000\004\000\
\005\000\006\000\007\000\008\000\000\000\009\000\000\000\000\000\
\000\000\010\000\000\000\000\000\011\000\012\000\013\000\014\000\
\004\000\005\000\006\000\007\000\008\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\012\000\013\000\
\014\000"

let yycheck = "\001\000\
\000\000\003\000\001\000\029\001\028\001\020\001\008\000\001\001\
\002\001\003\001\004\001\005\001\006\001\024\001\013\001\014\001\
\015\001\016\001\029\001\013\001\014\001\015\001\016\001\017\001\
\029\001\022\001\020\001\018\001\029\001\000\000\020\001\025\001\
\026\001\027\001\028\001\029\001\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\045\000\046\000\255\255\255\255\049\000\
\255\255\051\000\255\255\255\255\054\000\055\000\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\071\000\255\255\073\000\
\074\000\255\255\255\255\255\255\078\000\001\001\002\001\003\001\
\004\001\005\001\006\001\000\000\008\001\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\255\255\255\255\023\001\255\255\025\001\026\001\027\001\
\028\001\029\001\255\255\255\255\000\000\255\255\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\008\001\009\001\255\255\
\011\001\255\255\013\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\255\255\255\255\023\001\000\000\025\001\026\001\
\027\001\028\001\029\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\001\001\002\001\003\001\004\001\005\001\
\006\001\000\000\008\001\255\255\010\001\011\001\000\000\013\001\
\014\001\015\001\016\001\017\001\255\255\019\001\020\001\255\255\
\255\255\023\001\255\255\025\001\026\001\027\001\028\001\029\001\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\011\001\000\000\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\255\255\025\001\026\001\027\001\028\001\029\001\000\000\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\008\001\255\255\
\255\255\011\001\255\255\013\001\014\001\015\001\016\001\017\001\
\000\000\019\001\020\001\021\001\000\000\023\001\255\255\025\001\
\026\001\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\255\255\023\001\
\255\255\025\001\026\001\027\001\028\001\029\001\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\008\001\255\255\255\255\
\011\001\255\255\013\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\255\255\255\255\023\001\255\255\025\001\026\001\
\027\001\028\001\029\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\255\255\023\001\255\255\025\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\020\001\
\021\001\255\255\023\001\255\255\025\001\001\001\002\001\003\001\
\004\001\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\020\001\021\001\255\255\023\001\255\255\025\001\001\001\002\001\
\003\001\004\001\255\255\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\255\255\018\001\
\019\001\020\001\021\001\255\255\023\001\255\255\025\001\001\001\
\002\001\255\255\009\001\010\001\255\255\012\001\008\001\009\001\
\010\001\011\001\012\001\018\001\255\255\255\255\021\001\255\255\
\018\001\019\001\020\001\021\001\002\001\023\001\255\255\025\001\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\255\255\018\001\019\001\020\001\021\001\
\255\255\023\001\255\255\025\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\020\001\021\001\255\255\023\001\255\255\025\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\020\001\021\001\255\255\023\001\255\255\025\001\
\008\001\009\001\010\001\011\001\012\001\009\001\010\001\255\255\
\012\001\255\255\018\001\019\001\255\255\021\001\018\001\023\001\
\255\255\021\001\009\001\010\001\255\255\012\001\009\001\010\001\
\255\255\012\001\255\255\018\001\255\255\255\255\021\001\018\001\
\255\255\255\255\021\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\008\001\255\255\255\255\011\001\255\255\013\001\
\014\001\015\001\016\001\017\001\255\255\019\001\020\001\021\001\
\255\255\023\001\255\255\025\001\026\001\027\001\028\001\029\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\008\001\
\255\255\255\255\011\001\255\255\013\001\014\001\015\001\016\001\
\017\001\255\255\019\001\020\001\255\255\255\255\023\001\255\255\
\025\001\026\001\027\001\028\001\029\001\001\001\002\001\003\001\
\004\001\005\001\006\001\001\001\255\255\003\001\004\001\005\001\
\006\001\013\001\014\001\015\001\016\001\017\001\255\255\013\001\
\014\001\015\001\016\001\017\001\255\255\255\255\026\001\027\001\
\028\001\029\001\255\255\255\255\026\001\027\001\028\001\029\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\255\255\004\001\255\255\255\255\255\255\008\001\026\001\
\027\001\028\001\029\001\013\001\014\001\015\001\016\001\017\001\
\255\255\019\001\255\255\255\255\255\255\023\001\005\001\006\001\
\026\001\027\001\028\001\029\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\008\001\026\001\027\001\028\001\029\001\013\001\
\014\001\015\001\016\001\017\001\255\255\019\001\255\255\255\255\
\255\255\023\001\255\255\255\255\026\001\027\001\028\001\029\001\
\013\001\014\001\015\001\016\001\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\026\001\027\001\028\001\
\029\001"

let yynames_const = "\
  LAND\000\
  LOR\000\
  PLUS\000\
  MINUS\000\
  DIV\000\
  MULT\000\
  F\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SEMICOLON\000\
  COMMA\000\
  PRINT_INT\000\
  PRINT_BOOL\000\
  FIRST\000\
  SECOND\000\
  LBRACKET\000\
  RBRACKET\000\
  LET\000\
  EQUALS\000\
  IN\000\
  ARROW\000\
  FUN\000\
  REC\000\
  GREATER\000\
  TRUE\000\
  FALSE\000\
  EOF\000\
  "

let yynames_block = "\
  INTEGER\000\
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    Obj.repr(
# 39 "ml_parser.mly"
                          (Program(_1))
# 368 "ml_parser.ml"
               : Ml_syntax.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'local_definition) in
    Obj.repr(
# 43 "ml_parser.mly"
                    (_1)
# 375 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'recursive_function) in
    Obj.repr(
# 44 "ml_parser.mly"
                      (_1)
# 382 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'anonymous_function) in
    Obj.repr(
# 45 "ml_parser.mly"
                      (_1)
# 389 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_application) in
    Obj.repr(
# 46 "ml_parser.mly"
                        (_1)
# 396 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 47 "ml_parser.mly"
                    (_1)
# 403 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_block) in
    Obj.repr(
# 48 "ml_parser.mly"
                    (_1)
# 410 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'keywords) in
    Obj.repr(
# 49 "ml_parser.mly"
            (_1)
# 417 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 50 "ml_parser.mly"
            (_1)
# 424 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 51 "ml_parser.mly"
            (_1)
# 431 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 52 "ml_parser.mly"
             (_1)
# 438 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pair) in
    Obj.repr(
# 53 "ml_parser.mly"
        (_1)
# 445 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    Obj.repr(
# 54 "ml_parser.mly"
                                        (_2)
# 452 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'program_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    Obj.repr(
# 57 "ml_parser.mly"
                                                                 ( Pair(_2,_4) )
# 460 "ml_parser.ml"
               : 'pair))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "ml_parser.mly"
                        ( Keyword(Print_int) )
# 466 "ml_parser.ml"
               : 'keywords))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "ml_parser.mly"
                         ( Keyword(Print_bool) )
# 472 "ml_parser.ml"
               : 'keywords))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "ml_parser.mly"
                    ( Keyword(First) )
# 478 "ml_parser.ml"
               : 'keywords))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "ml_parser.mly"
                     ( Keyword(Second) )
# 484 "ml_parser.ml"
               : 'keywords))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 66 "ml_parser.mly"
                                                   ( Expression_block(_1,_3) )
# 492 "ml_parser.ml"
               : 'expression_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'program_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 69 "ml_parser.mly"
                                                                                  (If_then_else(_2,_4,_6))
# 501 "ml_parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 72 "ml_parser.mly"
                                                                            ( Local_definition(_2,_4,_6) )
# 510 "ml_parser.ml"
               : 'local_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 75 "ml_parser.mly"
                                                                                           ( Recursive_function(_3,_4,_6,_8) )
# 520 "ml_parser.ml"
               : 'recursive_function))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 78 "ml_parser.mly"
                                                     ( Anonymous_function(_2,_4) )
# 528 "ml_parser.ml"
               : 'anonymous_function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 81 "ml_parser.mly"
                                                     (Function_application(_1,_2))
# 536 "ml_parser.ml"
               : 'function_application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 84 "ml_parser.mly"
                     (_1)
# 543 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 85 "ml_parser.mly"
                        (_1)
# 550 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 88 "ml_parser.mly"
                                              ( Binary(LAnd,_1,_3) )
# 558 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 89 "ml_parser.mly"
                                             ( Binary(LOr,_1,_3) )
# 566 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 90 "ml_parser.mly"
                                                (Binary(Equals,_1,_3))
# 574 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 91 "ml_parser.mly"
                                                 (Binary(Greater,_1,_3))
# 582 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 94 "ml_parser.mly"
                                              ( Binary(Plus,_1,_3) )
# 590 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 95 "ml_parser.mly"
                                               ( Binary(Minus,_1,_3) )
# 598 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 96 "ml_parser.mly"
                                              ( Binary(Mult,_1,_3) )
# 606 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 97 "ml_parser.mly"
                                             (Binary(Div,_1,_3) )
# 614 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "ml_parser.mly"
            (Variable _1)
# 621 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean_constant) in
    Obj.repr(
# 103 "ml_parser.mly"
                    (_1)
# 628 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer_constant) in
    Obj.repr(
# 104 "ml_parser.mly"
                    (_1)
# 635 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "ml_parser.mly"
        (Boolean_constant true)
# 641 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "ml_parser.mly"
         (Boolean_constant false)
# 647 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "ml_parser.mly"
           (Integer_constant _1)
# 654 "ml_parser.ml"
               : 'integer_constant))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 112 "ml_parser.mly"
                                   (Integer_constant (-_3))
# 661 "ml_parser.ml"
               : 'integer_constant))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ml_syntax.program)
