open Ml_syntax

type ml_types = Int | Bool | Function of ml_types*ml_types | Type_variable of int | Universal of ml_types