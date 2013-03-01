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
	is parsed to obtain the AST. A conditional transformation and the De
	Bruijn index transformation will then be applied on the AST. Afterwards
	the program will be typed and the result will be printed on stdout. The
	program will then be interpreted and the result printed on stdout. Finally
	the program will be compiled and executed on the stack machine which was
	defined in the problem specification.
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
       (print_string "Parsed AST...\n";
				Helper.prettyPrintAST ml_prog;
        let e_prog = Elaboration.conditionalTransform ml_prog
        in
          (print_string "Conditionally transformed AST...\n";
					Helper.prettyPrintAST e_prog;
           let deBruijn_prog = Elaboration.deBruijnTransform e_prog
           in
             (print_string "De Bruijn transformed AST...\n";
							Helper.prettyPrintAST deBruijn_prog;
              print_string "Typing program...\n";
              let prog_type = Typing.program_type deBruijn_prog
              in
                (Helper.prettyPrintExpType prog_type true;
                 print_string "Interpreting program...\n";
                 let prog_result = Interpretation.interprete deBruijn_prog
                 in
                   (Helper.prettyPrintInterpretationResult 0 prog_result;
                    print_string "Compiling...\n";
                    let prog_compiled = Compiler.compileProgram deBruijn_prog
                    in
                      (print_string
                         ((StackMachine.string_of_commands prog_compiled) ^
                            "\n");
                       print_string "Running stack machine...\n";
                       StackMachine.execute prog_compiled)))))))
  
let _ = main ()
  

