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

(** Top-level AST for Litmus files *)

(** [t] is the top-level container for a Litmus test. *)

type 'a t

(** [pp ppa f litmus] pretty-prints the litmus test [litmus] onto
   formatter [f], given a program pretty printer [ppa]. *)

val pp : (Format.formatter -> 'a -> unit)
         -> Format.formatter
         -> 'a t
         -> unit

(** [err] is the type of validity errors that can arise when building
   a [t]. *)

type err =
  | NameEmpty
  | ProgramsEmpty
  | ProgramsNotUniform

(** [pp_err f err] pretty-prints the validity error [err] onto
   formatter [f]. *)

val pp_err : Format.formatter -> err -> unit

(** [make] tries to build a [t] from a name [name], language [lang], and
    program list [programs].  It returns a result with possible error [err]. *)
val make : name:string
           -> lang:Language.t
           -> programs:'a list list
           -> ('a t, err) result
