structure GetOpt :> GETOPT = struct
  datatype opt = FlagOpt of char
               | StrOpt of char
               | IntOpt of char

  datatype value = Flag of char
                 | Str of char * string
                 | Int of char * int

  exception MissingArgument of char
  exception UnknownOption of char
  exception InvalidArgument of char * string

  fun isOptArg arg = String.isPrefix "-" arg

  fun charOfOpt opt =
        case opt of FlagOpt ch => ch | StrOpt ch => ch | IntOpt ch => ch

  fun getopt opts f init args =
       let
         open Substring (* for triml, full, getc, isEmpty, string *)
         fun initial [] acc = (acc, [])
           | initial ("--"::args) acc = (acc, args)
           | initial (args as arg::args') acc =
               if isOptArg arg then readOptChar ((triml 1 o full) arg) args' acc
               else (acc, args)
         and readOptChar arg args acc =
               case getc arg of
                    NONE => initial args acc
                  | SOME (ch, arg) =>
                      case List.find (fn opt => charOfOpt opt = ch) opts of
                           NONE => raise UnknownOption ch
                         | SOME (FlagOpt ch) =>
                             readOptChar arg args (f (Flag ch, acc))
                         | SOME (StrOpt ch) =>
                             if isEmpty arg then
                               readArg ch (fn arg => Str (ch, arg)) args acc
                             else
                               initial args (f (Str (ch, string arg), acc))
                         | SOME (IntOpt ch) =>
                             let
                               fun cons arg =
                                 case Int.fromString arg of
                                      NONE => raise InvalidArgument (ch, arg)
                                    | SOME i => Int (ch, i)
                             in
                             if isEmpty arg then
                               readArg ch cons args acc
                             else
                               initial args (f (cons (string arg), acc))
                             end
         and readArg ch cons [] acc = raise MissingArgument ch
           | readArg ch cons (arg::args) acc = initial args (f (cons arg, acc))
       in
         initial args init
       end
end
