open Ml_syntax
  
type ml_types =
  | Int
  | Bool
	| Unit
  | Function of ml_types * ml_types
	| PairType of ml_types * ml_types
  | Type_variable of int
	(* Universal quantifying over all type variables. This is needed for the *)
	(* Hindley-Milner algorithm. *)
  | Universal of ml_types


