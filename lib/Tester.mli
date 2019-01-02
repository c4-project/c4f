(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Tester] provides the main `act` compiler tester, as used in the
    `act test` subcommand.

    The modules described in [Tester] run one or more compilers,
    across one or more machines, on a set of Memalloy-style C
    test cases with corresponding litmus files.  They then convert
    the resulting assembly into a litmus test, and (optionally)
    run Herd on the two tests, comparing the state sets emitted.

    These modules take the actual components used to do the testing---
    compilers, assembly job runners, and various other pieces of
    configuration---as parameters.  In `act`, most of these are
    filled in at the top-level, ie [Bin]. *)

include module type of Tester_intf

module Make_compiler (B : Basic_compiler) : Compiler
(** [Make_compiler] makes a single-compiler test runner from a
   [Basic_compiler]. *)

module Make_machine (B : Basic_machine) : Machine
(** [Make_machine] makes a single-machine test runner from a
   [Basic_machine]. *)

