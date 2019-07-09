#!/usr/bin/awk -f
#
# Takes a series of files representing threads of input to splice into a
# Litmus C file, then the file itself, and splices each thread over the
# corresponding assembly directive in the Litmus file.
#
# In the threads-to-splice part of the input, comments starting with // NEXT
# move to the next thread, and a comment starting with // END finishes splice
# input and begins the target Litmus file.

BEGIN {
  # The current thread being captured or spliced.
  thread = 0;
  filesize[0] = 0;

  # States of the splicer.
  S_CAPTURING = 0;
  S_CHANGING_THREAD = 1;
  S_ABOUT_TO_SPLICE = 2;
  S_SPLICING_OUTER = 3;
  S_SPLICING_INNER = 4;
  S_SPLICING_IN_PROGRESS = 5;
  state = S_CAPTURING;
}

# End of thread.
state == S_CAPTURING && /^\/\/ NEXT/ {
  state = S_CHANGING_THREAD;

  thread++;
  filesize[thread] = 0;
}

# End of splice input.
state == S_CAPTURING && /^\/\/ END/ {
  state = S_ABOUT_TO_SPLICE;
  thread = 0;
}

# Capture each line of each non-Litmus thread for later splicing.
state == S_CAPTURING {
  file[thread,filesize[thread]] = $0;
  filesize[thread]++;
}

# Assuming Litmus lays out its threads in sequential order.
state == S_SPLICING_OUTER && /static void \*P[0-9]+\(void \*_vb\) \{/ {
  state = S_SPLICING_INNER;
}

# Start of region to splice out.
state == S_SPLICING_INNER && /^asm __volatile__ \($/ {
  state = S_SPLICING_IN_PROGRESS;

  fs = filesize[thread];
  for (i = 0; i < fs; i++) print file[thread,i];
}

# End of thread harness function.
state == S_SPLICING_INNER && /return NULL;/ {
  state = S_SPLICING_OUTER;
  thread++;
}

# Print bits of harness that aren't to be spliced over.
state == S_SPLICING_OUTER || state == S_SPLICING_INNER {
  print $0;
}

# This goes after the catch-all print statement to avoid accidentally printing
# the ); when moving to S_SPLICING_INNER.
state == S_SPLICING_IN_PROGRESS && /^\);$/ {
  state = S_SPLICING_INNER;
}

# Making sure we properly skip over the special comments when capturing:
# we don't want to either capture them (// NEXT) or emit them (// END).
state == S_CHANGING_THREAD {
  state = S_CAPTURING;
}
state == S_ABOUT_TO_SPLICE {
  state = S_SPLICING_OUTER;
}

