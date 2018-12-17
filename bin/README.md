# `bin`: `act` toplevel

The top-level parts of `act` live here, including:

- the entry point, `act.ml`;
- the sub-command implementations;
- glue code to connect the top-level to the language and compiler
  specific modules;
- various example files (placed here so the regression tester can
  pick them up).

Most of the interesting parts of `act` live in `lib` (if they're
language-independent or core), or in target-specific modules.
