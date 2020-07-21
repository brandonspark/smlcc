structure Parser : PARSER =
struct
  structure Lexer = Lexer
  structure L = Lexer

  datatype exp = Const of int
  datatype statement = Return of exp
  datatype fundecl = Function of string * string * statement
  datatype program = Prog of fundecl

  exception ParseError

  fun fail () = raise ParseError

  fun find_semicolon (L : L.token list) 
                     (sc : L.token list -> 'a)
                     (fc : unit -> 'a) : 'a =
    case L of
        L.Semicolon::ts => sc ts
      | _ => fc ()

  fun find_brace (L : L.token list)
                 (sc : L.token list -> 'a)
                 (fc : unit -> 'a) : 'a =
    case L of
        L.RightBrace::ts => sc ts
      | _ => fc ()

  fun parse_exp (L : L.token list) 
                (sc : exp * L.token list -> 'a) 
                (fc : unit -> 'a) : 'a =
    case L of
        [] => fc ()
      | (L.Integer n)::ts => sc (Const n, ts)

  fun parse_statement (L : L.token list) 
                      (sc : statement * L.token list -> 'a) 
                      (fc : unit -> 'a) : 'a = 
    case L of
        (L.Keyword "return")::ts => 
          parse_exp ts (fn (exp, L') => 
            find_semicolon L' (fn L'' => sc (Return exp, L'')) fc
          ) fc
      | _ => fc ()

  fun parse_fundecl (L : L.token list)
              (sc : fundecl * L.token list -> 'a)
              (fc : unit -> 'a) : 'a =
    case L of
        ((L.Keyword "int")::(L.Identifier s)::(L.LeftParen)::(L.RightParen)
        ::(L.LeftBrace)::ts) =>
          (parse_statement ts (fn (statement, L') =>
            find_brace L' (fn L'' => sc (Function ("int", s, statement), L'')) fc
          ) fc)
      | _ => fc ()

  fun parse_program (L : L.token list)
                    (sc : program * L.token list -> 'a)
                    (fc : unit -> 'a) : 'a =
    parse_fundecl L (fn (fundecl, L') => sc (Prog fundecl, L')) fc

            
  fun parse (L : Lexer.token list) : program =
    case parse_program L SOME (fn () => NONE) of
         SOME (p, []) => p
       | _ => raise ParseError

  fun parseFile (s : string) : program =
    let
      val tokenlist = Lexer.lex s
    in
      parse tokenlist
    end
end
