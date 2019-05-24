#!/usr/bin/env awk -f
#
# Given an assembly Litmus test, replaces the body of the test with
# the contents of the externally provided variable `blank`.

BEGIN {
  FS = "|";
  OFS = "|";

  # States
  S_PREAMBLE  = 0;
  S_BODY      = 1;
  S_POSTAMBLE = 2;

  # Initial state
  state = S_PREAMBLE;
}

# Start of body
state == S_PREAMBLE && /^ *P0/ {
  state = S_BODY;

  # Reconstruct the header and blank bodies with the correct spacing.
  blen = length(blank);
  for (i = 1; i <= NF; i++) {
    head = $i;
    gsub(/ *;?/, "", head);
    printf " %-*s", blen, head;

    if (i == NF) {
      printf " ;\n";
    } else {
      printf " %s", OFS;
    }
  }
  for (i = 1; i < NF; i++) {
    printf " %s %s", blank, OFS;
  }
  printf " %s ;\n", blank;
}

# Ignore the rest of the body until we run out of it.
state == S_BODY && !/;$/ {
  state = S_POSTAMBLE;
}

# We always print the preamble and postamble.
state == S_PREAMBLE || state == S_POSTAMBLE {
  print $0;
}
