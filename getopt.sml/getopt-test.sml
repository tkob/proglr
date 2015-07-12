structure GetOptTest = struct
  open GetOpt

  val opts1 = [FlagOpt #"a", FlagOpt #"b", StrOpt #"f", StrOpt #"o"]
  val tests = [
    (opts1,
     "-ao arg path path",
     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"])),
    (opts1,
     "-a -o arg path path",
     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"])),
    (opts1,
     "-o arg -a path path",
     ([Flag #"a", Str (#"o", "arg")], ["path", "path"])),
    (opts1,
     "-a -o arg -- path path",
     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"])),
    (opts1,
     "-a -oarg path path",
     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"])),
    (opts1,
     "-aoarg path path",
     ([Str (#"o", "arg"), Flag #"a"], ["path", "path"])),

    (opts1, "", ([], []))
  ]

  fun test (opts, args, expected) =
        let
          val args = String.tokens (fn ch => ch = #" ") args
          val actual = getopt opts (List.::) [] args
        in
          if actual = expected then print "."
          else print "E"
        end

  fun run () = (List.app test tests; print "\n")

end

fun main () = GetOptTest.run ()
