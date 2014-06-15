#!/bin/sh
# vim: set filetype=tcl : \
exec expect -f "$0" ${1+"$@"}

set prompt "\n> "
set errmsg "Error"

spawn poly
expect -- $prompt

set errorOccurred 0

foreach source $argv {
  send "use \"$source\";\r"
  expect {
    $errmsg {
      set errorOccurred 1
      break
    }
    -- $prompt {}
  }
}

if {!$errorOccurred} {
  send "PolyML.export(\"a\", main);\r"
  expect {
    $errmsg {
      set errorOccurred 1
    }
    -- $prompt {}
  }
}
 
send "\004"
expect eof
exit $errorOccurred
