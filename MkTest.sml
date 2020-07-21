structure Test =
struct
  fun main (prog_name, args) =
    let
      val _::dirname::filename::xs = args
      val name = String.substring (filename, 0, (String.size filename) - 2)
      val () = print dirname
      val () = print name
      val _ = Generator.gen (dirname ^ "/" ^ filename, dirname ^ "/" ^ name ^ ".s")
    in
      1
    end
end
