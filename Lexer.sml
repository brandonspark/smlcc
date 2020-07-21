fun printList (ss : string list) : unit =
  let
    val combined = List.foldr (fn (s, rest) => s ^ ", " ^ rest) "" ss
    val combined' = String.substring (combined, 0, String.size combined - 2)
    val final = "[" ^ combined' ^ "]\n"
  in
    print final
  end

structure Lexer : LEXER =
struct
  datatype token = Keyword of string
                 | LeftParen
                 | RightParen
                 | LeftBrace
                 | RightBrace
                 | Integer of int
                 | Identifier of string
                 | Semicolon
  type t = token
  
  infixr $
  fun f $ x = f x

  (* wipe filename creates a single string of all the program text, with
   * newlines and spaces removed. *)
  fun wipe (filename : string) : string =
    let
      val inputfd = TextIO.openIn filename
      fun gen (instream : TextIO.instream) : string =
        let
          val newLine = TextIO.inputLine instream
        in
          case newLine of
            NONE => ""
          | SOME s => s ^ (gen instream)
        end
      val fullstr = gen inputfd
      val () = print fullstr
      fun clean (progstr : string) : string =
        let
          val tokenized = String.explode progstr
          fun tremove ([], acc) = acc
            | tremove ((#" ")::(#" ")::xs, acc) = tremove(#" "::xs, acc)
            | tremove (x::xs, acc) = tremove (xs, x::acc)
          fun spaceOut ([], acc) = acc
            | spaceOut (x::xs, acc) = 
              case x of
                  (#"(" | #")" | #"{" | #"}" | #";" | #"=") => 
                    spaceOut (xs, (#" ")::x::(#" ")::acc)
                 | #"\n" => spaceOut (xs, (#" ")::acc)
                 | _ => spaceOut (xs, x::acc)
          fun removeLast ([], acc) = acc
            | removeLast ([x], acc) = (case x of
                                           (#" " | #"\n") => acc
                                         | _ => x::acc)
            | removeLast (x::xs, acc) = removeLast (xs, x::acc)
        in
          String.implode $ List.rev $ removeLast (tremove(spaceOut (tokenized, []), []),
          [])
        end
    in
      clean fullstr
    end
  
  fun stringPred (f : char -> bool) (s : string) =
    List.foldr (fn (x, y) => x andalso y) true (List.map f  $ String.explode s)

  exception NoMatch

  fun match (s: string) : token =
    case s of
         "{" => LeftBrace
       | "}" => RightBrace
       | "(" => LeftParen
       | ")" => RightParen
       | ";" => Semicolon
       | "return" => Keyword "return"
       | "int" => Keyword "int"
       | _ => case (stringPred Char.isAlpha s, stringPred Char.isDigit s) of
                        (true, _) => Identifier s
                      | (_, true) => Integer (valOf $ Int.fromString s)
                      | _ => raise NoMatch

  fun lex (s : string) : token list = 
    let
      val tokens = String.tokens (fn #" " => true | _ => false) (wipe s) 
      val () = printList tokens
    in 
      List.map match tokens
    end

end
