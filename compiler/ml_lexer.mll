{

open Ml_parser

}

let digit = ['0'-'9']

let integer = digit+

let newline = "\n" | "\r" | "\r\n"

let nonDigit = ['a'-'z' '_' 'A'-'Z']
let identifier = nonDigit (nonDigit | digit)*

rule token = parse
	newline
	| [' ' '\t'] { token lexbuf }
	| "let" {LET}
	| "in" {IN}
	| "fun" {FUN}
	| "rec" {REC}
	| "->" {ARROW}
	| "=" {EQUALS}
	| "(" {LBRACKET}
	| ")" {RBRACKET}
	| "+" {PLUS}
	| "-" {MINUS}
	| "*" {MULT}
	| "/" {DIV}
	| "&&" {LAND}
	| "||" {LOR}
	| "true" {TRUE}
	| "false" {FALSE}
	| "if" {IF}
	| "then" {THEN}
	| "else" {ELSE}
	| ">" {GREATER}
	| ";" {SEMICOLON}
	| "," {COMMA}
	| "print_int" {PRINT_INT}
	| "print_bool" {PRINT_BOOL}
	| "fst" {FIRST}
	| "snd" {SECOND}
	| identifier {IDENTIFIER(Lexing.lexeme lexbuf)}
	| integer {INTEGER(int_of_string (Lexing.lexeme lexbuf))}
	| eof { EOF }
