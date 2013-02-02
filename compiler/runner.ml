open Ml_syntax

exception Error of exn * (int * int * string)

let main() =
	(* Parsing arguments *)
  	let f_name = ref "" in
  	Arg.parse [ ] (fun s -> f_name := s) "ML compiler";
  	(* Parsing of the source file *)
  	if String.compare !f_name "" = 0 then failwith "no program file given";
   	let f_desc = open_in !f_name in
		let lexbuf = Lexing.from_channel f_desc in
		let ml_prog =
		try Ml_parser.program Ml_lexer.token lexbuf
  			with
  			| e -> Printf.printf "Exception during parsing: %s\n" (Printexc.to_string e);
  				let curr = lexbuf.Lexing.lex_curr_p in
			let line = curr.Lexing.pos_lnum in
			let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
			let tok = Lexing.lexeme lexbuf in
    			raise (Error (e,(line,cnum,tok))) 
  		in
  		Helper.prettyPrintAST ml_prog;
  		let e_prog = Elaboration.conditionalTransform ml_prog in
  		Helper.prettyPrintAST e_prog;
  		let deBruijn_prog = Elaboration.deBruijnTransform e_prog in
  		Helper.prettyPrintAST deBruijn_prog;
			print_string "Typing program...\n";
			let prog_type = Typing.program_type deBruijn_prog in
			Helper.prettyPrintExpType prog_type true;
			print_string "Interpreting program...\n";
			let prog_result = Interpretation.interprete deBruijn_prog in
  		Helper.prettyPrintInterpretationResult 0 prog_result;
			print_string "Compiling...\n";
			let prog_compiled = Compiler.compileProgram deBruijn_prog in
			print_string ((StackMachine.string_of_commands prog_compiled)^"\n");
			print_string "Running stack machine...\n";
			StackMachine.execute prog_compiled;
			Printf.printf "finished yeah...\n"
    		
let _ = main()

