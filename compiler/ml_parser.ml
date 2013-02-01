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

(* | binary_operation {$1}| variable {$1}| constant {$1}     *)

# 35 "ml_parser.ml"
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
  267 (* LBRACKET *);
  268 (* RBRACKET *);
  269 (* LET *);
  270 (* EQUALS *);
  271 (* IN *);
  272 (* ARROW *);
  273 (* FUN *);
  274 (* REC *);
  275 (* GREATER *);
  276 (* TRUE *);
  277 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  278 (* INTEGER *);
  279 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\012\000\008\000\009\000\010\000\011\000\006\000\
\006\000\005\000\005\000\005\000\005\000\004\000\004\000\004\000\
\004\000\003\000\002\000\002\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\006\000\006\000\008\000\004\000\002\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\001\000\001\000\001\000\001\000\001\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\030\000\
\031\000\026\000\033\000\009\000\008\000\017\000\016\000\007\000\
\000\000\003\000\004\000\005\000\006\000\010\000\027\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\015\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\015\000\016\000\040\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000"

let yysindex = "\003\000\
\190\000\000\000\190\000\170\000\251\254\238\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\255\240\254\065\255\249\254\007\255\023\255\190\000\190\000\
\190\000\190\000\190\000\190\000\190\000\190\000\000\000\000\000\
\190\000\010\255\000\000\018\255\190\000\190\000\233\255\220\255\
\174\000\174\000\027\255\027\255\207\255\207\255\088\255\000\000\
\028\255\114\255\184\255\190\000\190\000\190\000\184\255\161\255\
\184\255\190\000\184\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\100\000\114\000\
\062\000\081\000\024\000\043\000\126\000\138\000\000\000\000\000\
\000\000\000\000\149\000\000\000\000\000\000\000\153\000\000\000\
\157\000\000\000\161\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 469
let yytable = "\017\000\
\039\000\025\000\027\000\001\000\030\000\042\000\031\000\032\000\
\033\000\034\000\035\000\036\000\028\000\003\000\041\000\044\000\
\004\000\029\000\005\000\037\000\045\000\056\000\006\000\025\000\
\038\000\007\000\008\000\009\000\010\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\004\000\046\000\055\000\
\057\000\061\000\024\000\058\000\059\000\000\000\007\000\008\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\063\000\064\000\065\000\022\000\000\000\000\000\
\067\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\003\000\000\000\000\000\004\000\043\000\005\000\037\000\000\000\
\023\000\006\000\000\000\038\000\007\000\008\000\009\000\010\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\003\000\
\000\000\060\000\004\000\018\000\005\000\037\000\000\000\000\000\
\006\000\000\000\038\000\007\000\008\000\009\000\010\000\000\000\
\000\000\019\000\031\000\032\000\033\000\034\000\035\000\036\000\
\000\000\003\000\000\000\000\000\004\000\020\000\005\000\037\000\
\062\000\000\000\006\000\000\000\038\000\007\000\008\000\009\000\
\010\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\013\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\003\000\000\000\000\000\004\000\000\000\005\000\037\000\066\000\
\000\000\006\000\000\000\038\000\007\000\008\000\009\000\010\000\
\031\000\032\000\033\000\034\000\035\000\036\000\000\000\003\000\
\000\000\000\000\004\000\000\000\005\000\037\000\000\000\000\000\
\006\000\000\000\038\000\007\000\008\000\009\000\010\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\031\000\000\000\033\000\034\000\
\035\000\036\000\007\000\008\000\009\000\010\000\004\000\000\000\
\000\000\000\000\000\000\033\000\034\000\035\000\036\000\007\000\
\008\000\009\000\010\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\008\000\009\000\010\000\
\000\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\003\000\000\000\000\000\004\000\000\000\005\000\037\000\000\000\
\000\000\006\000\000\000\038\000\007\000\008\000\009\000\010\000\
\025\000\025\000\025\000\025\000\025\000\025\000\000\000\025\000\
\025\000\025\000\000\000\025\000\025\000\025\000\025\000\000\000\
\025\000\000\000\025\000\024\000\024\000\024\000\024\000\024\000\
\024\000\000\000\024\000\024\000\024\000\000\000\024\000\024\000\
\024\000\024\000\000\000\024\000\000\000\024\000\022\000\022\000\
\022\000\022\000\000\000\000\000\000\000\022\000\022\000\022\000\
\000\000\022\000\022\000\022\000\022\000\000\000\022\000\000\000\
\022\000\023\000\023\000\023\000\023\000\000\000\000\000\000\000\
\023\000\023\000\023\000\000\000\023\000\023\000\023\000\023\000\
\000\000\023\000\000\000\023\000\018\000\018\000\000\000\000\000\
\000\000\000\000\000\000\018\000\018\000\018\000\000\000\018\000\
\018\000\018\000\018\000\019\000\018\000\000\000\018\000\000\000\
\000\000\019\000\019\000\019\000\000\000\019\000\019\000\019\000\
\019\000\000\000\019\000\000\000\019\000\020\000\020\000\020\000\
\000\000\020\000\020\000\020\000\020\000\000\000\020\000\000\000\
\020\000\021\000\021\000\021\000\000\000\021\000\021\000\021\000\
\021\000\000\000\021\000\000\000\021\000\014\000\014\000\000\000\
\014\000\011\000\011\000\014\000\011\000\012\000\012\000\011\000\
\012\000\013\000\013\000\012\000\013\000\026\000\000\000\013\000\
\000\000\003\000\035\000\036\000\004\000\000\000\005\000\000\000\
\004\000\000\000\006\000\000\000\000\000\007\000\008\000\009\000\
\010\000\007\000\008\000\009\000\010\000\003\000\000\000\000\000\
\004\000\000\000\005\000\000\000\000\000\000\000\006\000\000\000\
\000\000\007\000\008\000\009\000\010\000"

let yycheck = "\001\000\
\000\000\003\000\004\000\001\000\023\001\022\001\001\001\002\001\
\003\001\004\001\005\001\006\001\018\001\008\001\009\001\023\001\
\011\001\023\001\013\001\014\001\014\001\012\001\017\001\000\000\
\019\001\020\001\021\001\022\001\023\001\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\011\001\016\001\041\000\
\023\001\014\001\000\000\045\000\046\000\255\255\020\001\021\001\
\022\001\023\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\060\000\061\000\062\000\000\000\255\255\255\255\
\066\000\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\000\000\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\008\001\
\255\255\010\001\011\001\000\000\013\001\014\001\255\255\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\000\000\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\008\001\255\255\255\255\011\001\000\000\013\001\014\001\
\015\001\255\255\017\001\255\255\019\001\020\001\021\001\022\001\
\023\001\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\000\000\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\011\001\255\255\013\001\014\001\015\001\
\255\255\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\008\001\
\255\255\255\255\011\001\255\255\013\001\014\001\255\255\255\255\
\017\001\255\255\019\001\020\001\021\001\022\001\023\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\011\001\255\255\255\255\001\001\255\255\003\001\004\001\
\005\001\006\001\020\001\021\001\022\001\023\001\011\001\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\020\001\
\021\001\022\001\023\001\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\011\001\255\255\013\001\014\001\255\255\
\255\255\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\255\255\
\017\001\255\255\019\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\255\255\017\001\255\255\019\001\001\001\002\001\
\003\001\004\001\255\255\255\255\255\255\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\255\255\017\001\255\255\
\019\001\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\255\255\017\001\255\255\019\001\001\001\002\001\255\255\255\255\
\255\255\255\255\255\255\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\002\001\017\001\255\255\019\001\255\255\
\255\255\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\255\255\017\001\255\255\019\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\255\255\017\001\255\255\
\019\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\255\255\017\001\255\255\019\001\009\001\010\001\255\255\
\012\001\009\001\010\001\015\001\012\001\009\001\010\001\015\001\
\012\001\009\001\010\001\015\001\012\001\004\001\255\255\015\001\
\255\255\008\001\005\001\006\001\011\001\255\255\013\001\255\255\
\011\001\255\255\017\001\255\255\255\255\020\001\021\001\022\001\
\023\001\020\001\021\001\022\001\023\001\008\001\255\255\255\255\
\011\001\255\255\013\001\255\255\255\255\255\255\017\001\255\255\
\255\255\020\001\021\001\022\001\023\001"

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
# 38 "ml_parser.mly"
                          (Program(_1))
# 281 "ml_parser.ml"
               : Ml_syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    Obj.repr(
# 42 "ml_parser.mly"
                                        (_2)
# 288 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'local_definition) in
    Obj.repr(
# 43 "ml_parser.mly"
                    (_1)
# 295 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'recursive_function) in
    Obj.repr(
# 44 "ml_parser.mly"
                      (_1)
# 302 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'anonymous_function) in
    Obj.repr(
# 45 "ml_parser.mly"
                      (_1)
# 309 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_application) in
    Obj.repr(
# 46 "ml_parser.mly"
                        (_1)
# 316 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 47 "ml_parser.mly"
                    (_1)
# 323 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 48 "ml_parser.mly"
            (_1)
# 330 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 49 "ml_parser.mly"
            (_1)
# 337 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 50 "ml_parser.mly"
             (_1)
# 344 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'program_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 53 "ml_parser.mly"
                                                                                  (If_then_else(_2,_4,_6))
# 353 "ml_parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 56 "ml_parser.mly"
                                                                            ( Local_definition(_2,_4,_6) )
# 362 "ml_parser.ml"
               : 'local_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 59 "ml_parser.mly"
                                                                                           ( Recursive_function(_3,_4,_6,_8) )
# 372 "ml_parser.ml"
               : 'recursive_function))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 62 "ml_parser.mly"
                                                     ( Anonymous_function(_2,_4) )
# 380 "ml_parser.ml"
               : 'anonymous_function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 65 "ml_parser.mly"
                                                     (Function_application(_1,_2))
# 388 "ml_parser.ml"
               : 'function_application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 68 "ml_parser.mly"
                     (_1)
# 395 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 69 "ml_parser.mly"
                        (_1)
# 402 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 72 "ml_parser.mly"
                                              ( Binary(LAnd,_1,_3) )
# 410 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 73 "ml_parser.mly"
                                             ( Binary(LOr,_1,_3) )
# 418 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 74 "ml_parser.mly"
                                                (Binary(Equals,_1,_3))
# 426 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 75 "ml_parser.mly"
                                                 (Binary(Greater,_1,_3))
# 434 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 78 "ml_parser.mly"
                                              ( Binary(Plus,_1,_3) )
# 442 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 79 "ml_parser.mly"
                                               ( Binary(Minus,_1,_3) )
# 450 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 80 "ml_parser.mly"
                                              ( Binary(Mult,_1,_3) )
# 458 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 81 "ml_parser.mly"
                                             (Binary(Div,_1,_3) )
# 466 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "ml_parser.mly"
            (Variable _1)
# 473 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean_constant) in
    Obj.repr(
# 87 "ml_parser.mly"
                    (_1)
# 480 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer_constant) in
    Obj.repr(
# 88 "ml_parser.mly"
                    (_1)
# 487 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "ml_parser.mly"
        (Boolean_constant true)
# 493 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "ml_parser.mly"
         (Boolean_constant false)
# 499 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "ml_parser.mly"
           (Integer_constant _1)
# 506 "ml_parser.ml"
               : 'integer_constant))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 96 "ml_parser.mly"
                                   (Integer_constant (-_3))
# 513 "ml_parser.ml"
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
