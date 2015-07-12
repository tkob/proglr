signature GETOPT = sig
  datatype opt = FlagOpt of char
               | StrOpt of char
               | IntOpt of char

  datatype value = Flag of char
                 | Str of char * string
                 | Int of char * int

  exception MissingArgument of char
  exception UnknownOption of char
  exception InvalidArgument of char * string

  val getopt :
    opt list -> (value * 'a -> 'a) -> 'a -> string list -> 'a * string list
end
