# `lib`: target-independent, mid-level parts of `act`

This part of `act` contains the majority of `act`'s language and
compiler-independent code, as well as interfaces and utilities used
by the target-specific parts.

Since `lib` is used by the target-specific parts of `act`, it can't
depend on any of them.  As a result, glue code to connect `act` to
the language and compiler implementations is in `bin`, not `lib`.
