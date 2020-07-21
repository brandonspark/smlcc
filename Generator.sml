
structure Generator : GENERATOR =
struct
  structure P = Parser
  open Format;
  fun gen (infile : string, outfile : string) : unit =
    let
      val outfd = TextIO.openOut outfile
      val AST = Parser.parseFile infile
      
      fun decompose_statement (P.Return (P.Const n) : P.statement) : unit =
        (TextIO.output (outfd, format "\tmovl\t$%d," [INT n] ^ (" %eax\n\tret\n")))
      
      fun decompose_fundecl (P.Function (ty, name, statement) : P.fundecl) : unit =
        (TextIO.output (outfd, format "\t.globl %s\n%s:\n" [STR name, STR name]); 
        decompose_statement statement)
      
      fun decompose_prog (P.Prog p : P.program) : unit = decompose_fundecl p
    in
      decompose_prog AST 
    end
end


