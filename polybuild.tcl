#!/bin/sh
# vim: set filetype=tcl : \
exec expect -f "$0" ${1+"$@"}

set timeout -1
set prompt "\n> "
set errmsg "Error"

if {[info exists env(DEBUG)] && $env(DEBUG)} {
  set debug 1
} else {
  set debug 0
}

spawn poly
expect -- $prompt

set errorOccurred 0

if {$debug} {
  send "PolyML.Compiler.debug := true;\r"
  expect -- $prompt
}

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

if {$debug} {
  send "open PolyML.Debug;\r"
  interact
  exit $errorOccurred
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
