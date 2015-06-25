fun readEscapeAndWrite (ins, outs) =
let
  val chunk = BinIO.input ins
  val escape = String.toString o Byte.bytesToString
in
  if Word8Vector.length chunk = 0 then ()
  else
    (TextIO.output (outs, escape chunk);
    readEscapeAndWrite (ins, outs))
end

fun writeResourceEntry (path, outs) =
let
  fun p s = TextIO.output (outs, s)
  val ins = BinIO.openIn path
in
  p "  (\"";
  p (String.toString path);
  p "\", \"";
  readEscapeAndWrite (ins, outs);
  p "\"),\n";
  BinIO.closeIn ins
end

fun writeStructure (str, paths, outs) =
let
  fun p s = TextIO.output (outs, s)
in
  p "structure ";
  p str;
  p " = struct\n";
  p "  val resources = [\n";
  List.app (fn path => writeResourceEntry (path, outs)) paths;
  p "    (\"\", \"\") ]\n";
  p "  fun get path =\n";
  p "    let fun get' [] = raise Empty\n";
  p "          | get' ((path', datum)::resources) =\n";
  p "              if path = path' then datum\n";
  p "              else get' resources\n";
  p "    in\n";
  p "      get' resources\n";
  p "    end\n";
  p "end"
end

fun main () =
let
  (* 1st argument is structure name *)
  val (str::paths) = CommandLine.arguments () 
in
  writeStructure (str, paths, TextIO.stdOut)
end
