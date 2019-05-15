#!/usr/bin/env gawk -f
#
# Given a series of files p0, p1, .., pn of assembly directives and a final
# Litmus C file, splices each file's content over the assembly directive
# in the Litmus file.
#
# Requires gawk.

BEGINFILE {
	# States of the splicer.
	IN_LITMUS = (ARGIND == ARGC - 1);
	IN_PROGRAM = 0;
	IS_SPLICING = 0;

	# The current program being spliced.
	# This is 1-indexed, so 0 represents the gap before the first program.
	PROGRAM = 0;

	FILESIZE[ARGIND] = 0;
}

# Capture each line of each non-Litmus program for later splicing.
!IN_LITMUS {
	FILE[ARGIND,FILESIZE[ARGIND]] = $0;
	FILESIZE[ARGIND]++;
}

# Assuming Litmus lays out its programs in sequential order.
IN_LITMUS && /static void \*P[0-9]+\(void \*_vb\) \{/ {
	IN_PROGRAM = 1;
	PROGRAM++;
}

IN_PROGRAM && /^asm __volatile__ \($/ {
	IS_SPLICING = 1;

	fs = FILESIZE[PROGRAM];
	for (i = 0; i < fs; i++) print FILE[PROGRAM,i];
}

IN_PROGRAM && /return NULL;/ {
	IN_PROGRAM = 0;
}

IN_LITMUS && !IS_SPLICING {
	print $0;
}

# This goes after the catch-all print statement to avoid accidentally printing
# the );.
IS_SPLICING && /^);$/ {
	IS_SPLICING = 0;
}
