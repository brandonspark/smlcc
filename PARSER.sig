signature PARSER =
sig 
  structure Lexer : LEXER

  datatype exp = Const of int
  datatype statement = Return of exp
  datatype fundecl = Function of string * string * statement (* type * name * statement *)
  datatype program = Prog of fundecl

  val parse : Lexer.token list -> program

  val parseFile : string -> program
end
