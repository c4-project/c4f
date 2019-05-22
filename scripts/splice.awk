#!/usr/bin/env gawk -f
#
# Given a series of files p0, p1, .., pn of assembly directives and a final
# Litmus C file, splices each file's content over the assembly directive
# in the Litmus file.  Changes in file, as well as comments starting with
# // NEXT, move to the next thread.
#
# Requires gawk.

BEGIN {
  # The current program being captured.
  # This is 1-indexed, so 0 represents the gap before the first program.
  program_in = 1;
}

BEGINFILE {
  # States of the splicer.
  in_litmus = (ARGIND == ARGC - 1);
  in_program = 0;
  is_splicing = 0;
  is_changing_program = 0;

  # The current program being spliced.
  # This is 1-indexed, so 0 represents the gap before the first program.
  program = 0;

  filesize[program_in] = 0;
}

# The two different ways to change program when capturing.
ENDFILE {
  if (!in_litmus) {
    program_in++;
  }
}
!in_litmus && /^\/\/ NEXT/ {
  is_changing_program = 1;
  program_in++;
}

# Capture each line of each non-Litmus program for later splicing.
!in_litmus {
  file[program_in,filesize[program_in]] = $0;
  filesize[program_in]++;
}

!in_litmus && is_changing_program {
  is_changing_program = 0;
}

# Assuming Litmus lays out its programs in sequential order.
in_litmus && /static void \*P[0-9]+\(void \*_vb\) \{/ {
  in_program = 1;
  program++;
}

in_program && /^asm __volatile__ \($/ {
  is_splicing = 1;

  fs = filesize[program];
  for (i = 0; i < fs; i++) print file[program,i];
}

in_program && /return NULL;/ {
  in_program = 0;
}

in_litmus && !is_splicing {
  print $0;
}

# This goes after the catch-all print statement to avoid accidentally printing
# the );.
is_splicing && /^);$/ {
  is_splicing = 0;
}
