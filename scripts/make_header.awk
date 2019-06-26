#!/usr/bin/awk -f
#
# Given an obj_splitmus style delitmusified C file, auto-generate
# a header file.
# This is heavily dependent on the particular indent style of said files
# and won't work for C files in general.

BEGIN {
  next_line_is_decl = 0;

  print "#include <stdatomic.h>"
}

next_line_is_decl {
  next_line_is_decl = 0;
  printf "void %s;\n", $0;
}

# Start of function
!next_line_is_decl && /^ *void/ {
  next_line_is_decl = 1;
}
