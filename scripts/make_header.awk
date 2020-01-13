#!/usr/bin/awk -f
#
# Given a c_litmus_indirect style delitmusified C file, auto-generate
# a header file.
# This is heavily dependent on the particular indent style of said files
# and won't work for C files in general.

BEGIN {
  in_decl = 0;

  print "#include <stdatomic.h>"
  print "#include <stdbool.h>"
}

# Start of function
!in_decl && /^ *void/ { in_decl = 1; }

# End of function
in_decl && /^ *\{/ {
  in_decl = 0;
  print ";";
}

in_decl { print $0; }
