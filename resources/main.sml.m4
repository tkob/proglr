structure Parse = ParseFun(Lexer)

open Parse.Ast

structure Main = struct
  fun main (name, arguments) =
       let
         val strm = Lexer.streamifyInstream TextIO.stdIn
         val sourcemap = AntlrStreamPos.mkSourcemap ()
         val trees = Parse.parse sourcemap strm
       in
         OS.Process.success
       end
end
