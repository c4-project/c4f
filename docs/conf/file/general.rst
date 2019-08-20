General format
--------------

- The configuration file consists of multiple items, each of which is either a
  line of configuration or a brace-delimited, named block.
- Each configuration line consists of a run of multiple keywords and arguments
  (the exact syntax depends on the line, but usually starts with one or more
  keywords).

  - Arguments, at time of writing, can be _identifiers_ (dot-separated,
    mostly-alphanumeric unquoted strings) or *string literals* (double-quoted).

- ``#`` is the comment character, and discards up to the end of the line.
- Lines containing only whitespace/comments are ignored.
