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

# 34 "ml_parser.ml"
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
  268 (* LBRACKET *);
  269 (* RBRACKET *);
  270 (* LET *);
  271 (* EQUALS *);
  272 (* IN *);
  273 (* ARROW *);
  274 (* FUN *);
  275 (* REC *);
  276 (* GREATER *);
  277 (* TRUE *);
  278 (* FALSE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  279 (* INTEGER *);
  280 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\012\000\013\000\008\000\009\000\010\000\
\011\000\006\000\006\000\005\000\005\000\005\000\005\000\004\000\
\004\000\004\000\004\000\003\000\002\000\002\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\006\000\006\000\008\000\004\000\
\002\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\032\000\
\033\000\028\000\035\000\010\000\009\000\019\000\018\000\007\000\
\000\000\003\000\004\000\005\000\006\000\008\000\011\000\029\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\017\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\014\000\015\000\016\000\042\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000"

let yysindex = "\003\000\
\216\000\000\000\216\000\036\255\032\255\237\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\255\239\254\068\255\245\254\001\255\002\255\216\000\
\216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
\000\000\000\000\216\000\009\255\000\000\255\254\216\000\216\000\
\233\255\195\000\199\000\199\000\127\255\127\255\205\255\229\255\
\229\255\092\255\000\000\026\255\120\255\181\255\216\000\216\000\
\216\000\181\255\152\255\181\255\216\000\181\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\105\000\120\000\065\000\085\000\025\000\045\000\159\000\133\000\
\146\000\000\000\000\000\000\000\000\000\169\000\000\000\000\000\
\000\000\171\000\000\000\179\000\000\000\181\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 496
let yytable = "\017\000\
\041\000\026\000\028\000\001\000\031\000\044\000\032\000\033\000\
\034\000\035\000\036\000\037\000\046\000\003\000\043\000\047\000\
\038\000\004\000\048\000\005\000\039\000\059\000\060\000\006\000\
\027\000\040\000\007\000\008\000\009\000\010\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\027\000\
\064\000\058\000\000\000\003\000\026\000\061\000\062\000\004\000\
\000\000\005\000\029\000\000\000\000\000\006\000\000\000\030\000\
\007\000\008\000\009\000\010\000\000\000\066\000\067\000\068\000\
\024\000\000\000\000\000\070\000\032\000\033\000\034\000\035\000\
\036\000\037\000\000\000\003\000\000\000\000\000\038\000\004\000\
\045\000\005\000\039\000\000\000\025\000\006\000\000\000\040\000\
\007\000\008\000\009\000\010\000\032\000\033\000\034\000\035\000\
\036\000\037\000\000\000\003\000\000\000\063\000\038\000\004\000\
\020\000\005\000\039\000\000\000\000\000\006\000\000\000\040\000\
\007\000\008\000\009\000\010\000\000\000\000\000\000\000\021\000\
\032\000\033\000\034\000\035\000\036\000\037\000\000\000\003\000\
\000\000\000\000\038\000\004\000\022\000\005\000\039\000\065\000\
\000\000\006\000\004\000\040\000\007\000\008\000\009\000\010\000\
\000\000\023\000\000\000\007\000\008\000\009\000\010\000\000\000\
\032\000\033\000\034\000\035\000\036\000\037\000\012\000\003\000\
\000\000\000\000\038\000\004\000\000\000\005\000\039\000\069\000\
\016\000\006\000\013\000\040\000\007\000\008\000\009\000\010\000\
\000\000\000\000\014\000\000\000\015\000\032\000\033\000\034\000\
\035\000\036\000\037\000\000\000\003\000\000\000\000\000\038\000\
\004\000\000\000\005\000\039\000\000\000\000\000\006\000\000\000\
\040\000\007\000\008\000\009\000\010\000\032\000\033\000\034\000\
\035\000\036\000\037\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\040\000\007\000\008\000\009\000\010\000\032\000\033\000\034\000\
\035\000\036\000\037\000\034\000\035\000\036\000\037\000\000\000\
\004\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\007\000\008\000\009\000\010\000\007\000\008\000\009\000\
\010\000\032\000\033\000\034\000\035\000\036\000\037\000\000\000\
\003\000\000\000\000\000\038\000\004\000\000\000\005\000\039\000\
\000\000\000\000\006\000\000\000\040\000\007\000\008\000\009\000\
\010\000\027\000\027\000\027\000\027\000\027\000\027\000\000\000\
\027\000\027\000\027\000\027\000\000\000\027\000\027\000\027\000\
\027\000\000\000\027\000\000\000\027\000\026\000\026\000\026\000\
\026\000\026\000\026\000\000\000\026\000\026\000\026\000\026\000\
\000\000\026\000\026\000\026\000\026\000\000\000\026\000\000\000\
\026\000\024\000\024\000\024\000\024\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\000\000\024\000\024\000\024\000\
\024\000\000\000\024\000\000\000\024\000\025\000\025\000\025\000\
\025\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\
\000\000\025\000\025\000\025\000\025\000\000\000\025\000\000\000\
\025\000\020\000\020\000\000\000\000\000\000\000\000\000\000\000\
\020\000\020\000\020\000\020\000\000\000\020\000\020\000\020\000\
\020\000\021\000\020\000\000\000\020\000\000\000\000\000\021\000\
\021\000\021\000\021\000\000\000\021\000\021\000\021\000\021\000\
\000\000\021\000\000\000\021\000\022\000\022\000\022\000\022\000\
\000\000\022\000\022\000\022\000\022\000\000\000\022\000\000\000\
\022\000\023\000\023\000\023\000\023\000\000\000\023\000\023\000\
\023\000\023\000\000\000\023\000\000\000\023\000\012\000\012\000\
\012\000\012\000\000\000\012\000\012\000\000\000\012\000\000\000\
\012\000\016\000\016\000\013\000\013\000\016\000\000\000\013\000\
\016\000\000\000\013\000\014\000\014\000\015\000\015\000\014\000\
\000\000\015\000\014\000\032\000\015\000\034\000\035\000\036\000\
\037\000\000\000\000\000\036\000\037\000\000\000\004\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\007\000\
\008\000\009\000\010\000\007\000\008\000\009\000\010\000\003\000\
\000\000\000\000\000\000\004\000\000\000\005\000\000\000\000\000\
\000\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000"

let yycheck = "\001\000\
\000\000\003\000\004\000\001\000\024\001\023\001\001\001\002\001\
\003\001\004\001\005\001\006\001\024\001\008\001\009\001\015\001\
\011\001\012\001\017\001\014\001\015\001\013\001\024\001\018\001\
\000\000\020\001\021\001\022\001\023\001\024\001\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\004\001\
\015\001\043\000\255\255\008\001\000\000\047\000\048\000\012\001\
\255\255\014\001\019\001\255\255\255\255\018\001\255\255\024\001\
\021\001\022\001\023\001\024\001\255\255\063\000\064\000\065\000\
\000\000\255\255\255\255\069\000\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\000\000\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\008\001\255\255\010\001\011\001\012\001\
\000\000\014\001\015\001\255\255\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\255\255\255\255\255\255\000\000\
\001\001\002\001\003\001\004\001\005\001\006\001\255\255\008\001\
\255\255\255\255\011\001\012\001\000\000\014\001\015\001\016\001\
\255\255\018\001\012\001\020\001\021\001\022\001\023\001\024\001\
\255\255\000\000\255\255\021\001\022\001\023\001\024\001\255\255\
\001\001\002\001\003\001\004\001\005\001\006\001\000\000\008\001\
\255\255\255\255\011\001\012\001\255\255\014\001\015\001\016\001\
\000\000\018\001\000\000\020\001\021\001\022\001\023\001\024\001\
\255\255\255\255\000\000\255\255\000\000\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\008\001\255\255\255\255\011\001\
\012\001\255\255\014\001\015\001\255\255\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\255\255\015\001\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\001\001\002\001\003\001\
\004\001\005\001\006\001\003\001\004\001\005\001\006\001\255\255\
\012\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\021\001\022\001\023\001\024\001\021\001\022\001\023\001\
\024\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\255\255\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\008\001\009\001\010\001\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\001\001\002\001\003\001\
\004\001\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\001\001\002\001\255\255\255\255\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\002\001\018\001\255\255\020\001\255\255\255\255\008\001\
\009\001\010\001\011\001\255\255\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\008\001\009\001\010\001\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\008\001\009\001\010\001\011\001\255\255\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\008\001\009\001\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\018\001\009\001\010\001\009\001\010\001\013\001\255\255\013\001\
\016\001\255\255\016\001\009\001\010\001\009\001\010\001\013\001\
\255\255\013\001\016\001\001\001\016\001\003\001\004\001\005\001\
\006\001\255\255\255\255\005\001\006\001\255\255\012\001\255\255\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\021\001\
\022\001\023\001\024\001\021\001\022\001\023\001\024\001\008\001\
\255\255\255\255\255\255\012\001\255\255\014\001\255\255\255\255\
\255\255\018\001\255\255\255\255\021\001\022\001\023\001\024\001"

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
# 288 "ml_parser.ml"
               : Ml_syntax.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    Obj.repr(
# 42 "ml_parser.mly"
                                        (_2)
# 295 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'local_definition) in
    Obj.repr(
# 43 "ml_parser.mly"
                    (_1)
# 302 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'recursive_function) in
    Obj.repr(
# 44 "ml_parser.mly"
                      (_1)
# 309 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'anonymous_function) in
    Obj.repr(
# 45 "ml_parser.mly"
                      (_1)
# 316 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_application) in
    Obj.repr(
# 46 "ml_parser.mly"
                        (_1)
# 323 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 47 "ml_parser.mly"
                    (_1)
# 330 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_block) in
    Obj.repr(
# 48 "ml_parser.mly"
                    (_1)
# 337 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 49 "ml_parser.mly"
            (_1)
# 344 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 50 "ml_parser.mly"
            (_1)
# 351 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 51 "ml_parser.mly"
             (_1)
# 358 "ml_parser.ml"
               : 'program_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 54 "ml_parser.mly"
                                                   ( Expression_block(_1,_3) )
# 366 "ml_parser.ml"
               : 'expression_block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'program_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 57 "ml_parser.mly"
                                                                                  (If_then_else(_2,_4,_6))
# 375 "ml_parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 60 "ml_parser.mly"
                                                                            ( Local_definition(_2,_4,_6) )
# 384 "ml_parser.ml"
               : 'local_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 63 "ml_parser.mly"
                                                                                           ( Recursive_function(_3,_4,_6,_8) )
# 394 "ml_parser.ml"
               : 'recursive_function))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 66 "ml_parser.mly"
                                                     ( Anonymous_function(_2,_4) )
# 402 "ml_parser.ml"
               : 'anonymous_function))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 69 "ml_parser.mly"
                                                     (Function_application(_1,_2))
# 410 "ml_parser.ml"
               : 'function_application))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 72 "ml_parser.mly"
                     (_1)
# 417 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ml_syntax.program_exp) in
    Obj.repr(
# 73 "ml_parser.mly"
                        (_1)
# 424 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 76 "ml_parser.mly"
                                              ( Binary(LAnd,_1,_3) )
# 432 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 77 "ml_parser.mly"
                                             ( Binary(LOr,_1,_3) )
# 440 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 78 "ml_parser.mly"
                                                (Binary(Equals,_1,_3))
# 448 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 79 "ml_parser.mly"
                                                 (Binary(Greater,_1,_3))
# 456 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 82 "ml_parser.mly"
                                              ( Binary(Plus,_1,_3) )
# 464 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 83 "ml_parser.mly"
                                               ( Binary(Minus,_1,_3) )
# 472 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 84 "ml_parser.mly"
                                              ( Binary(Mult,_1,_3) )
# 480 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'program_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program_expression) in
    Obj.repr(
# 85 "ml_parser.mly"
                                             (Binary(Div,_1,_3) )
# 488 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "ml_parser.mly"
            (Variable _1)
# 495 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean_constant) in
    Obj.repr(
# 91 "ml_parser.mly"
                    (_1)
# 502 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'integer_constant) in
    Obj.repr(
# 92 "ml_parser.mly"
                    (_1)
# 509 "ml_parser.ml"
               : Ml_syntax.program_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "ml_parser.mly"
        (Boolean_constant true)
# 515 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "ml_parser.mly"
         (Boolean_constant false)
# 521 "ml_parser.ml"
               : 'boolean_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "ml_parser.mly"
           (Integer_constant _1)
# 528 "ml_parser.ml"
               : 'integer_constant))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 100 "ml_parser.mly"
                                   (Integer_constant (-_3))
# 535 "ml_parser.ml"
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
