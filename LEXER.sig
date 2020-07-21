val () = Control.Print.stringDepth := 500
val () = Control.Print.printDepth := 100
val () = Control.Print.printLength := 500

signature LEXER =
sig
  datatype token = Keyword of string
                 | LeftParen
                 | RightParen
                 | LeftBrace
                 | RightBrace
                 | Integer of int
                 | Identifier of string
                 | Semicolon
  
  type t = token
  val wipe : string -> string
  val lex : string -> t list
end
