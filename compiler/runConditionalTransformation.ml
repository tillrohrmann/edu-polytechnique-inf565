open Ml_syntax

(** 
	Exception to report parsing errors
	
	@param 1st line number
	@param 2nd token number
	@param 3rd token
*)
exception Error of exn * (int * int * string)
  
(**
	Main function reads the file which is specified by Arg[0]. This file
	is parsed to obtain the AST. The conditional transformation is applied
	to the AST and the transformed version then printed.
*)
let main () = (* Parsing arguments *)
  let f_name = ref ""
  in
    (* Parsing of the source file *)
    (Arg.parse [] (fun s -> f_name := s) "ML compiler";
     if (String.compare !f_name "") = 0
     then failwith "no program file given"
     else ();
     let f_desc = open_in !f_name in
     let lexbuf = Lexing.from_channel f_desc in
     let ml_prog =
       try Ml_parser.program Ml_lexer.token lexbuf
       with
       | e ->
           (Printf.printf "Exception during parsing: %s\n"
              (Printexc.to_string e);
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf
            in raise (Error (e, (line, cnum, tok))))
     in
				print_string "Conditional transformation...\n";
        let e_prog = Elaboration.conditionalTransform ml_prog
        in
				Helper.prettyPrintAST e_prog)
  
let _ = main ()