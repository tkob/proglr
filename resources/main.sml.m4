structure Parse = ParseFun(Lexer)

open Parse.Ast

structure Main = struct
  fun main (name, arguments) =
       let
         val strm = Lexer.streamifyInstream TextIO.stdIn
         val sourcemap = AntlrStreamPos.mkSourcemap ()
         val trees = Parse.parse sourcemap strm
         val numParses = length trees
       in
         print (Int.toString numParses ^ " parse(s)\n");
         OS.Process.success
       end
end

ifelse(PROGLR_COMPILER,`mlton', `
fun main () =
  let
    val name = CommandLine.name ()
    val arguments = CommandLine.arguments ()
  in
      OS.Process.exit (Main.main (name, arguments))
  end

val _ = main ()
')
