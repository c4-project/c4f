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

(** Parsing and comparison functionality for Herd output

[Herd] contains functions for scraping the human-readable summary of a
   Herd7 run, extracting location information, and comparing states.
   *)

open Utils

(** [t] is the opaque type of a Herd output analysis. *)
type t;;

(** [single_outcome] is the type of outcomes we can get from single
   (or double) Herd runs. *)
type single_outcome =
  [ `Unknown   (** Either only one Herd run was analysed, or the
                   results are inconclusive. *)
  | `Undef     (** The final execution triggered undefined
                   behaviour. *)
  ]
[@@deriving sexp]
;;

(** [outcome] is the type of summaries of Herd analysis. *)
type outcome =
  [ single_outcome
  | `OracleUndef  (** The oracle execution triggered undefined
                      behaviour. *)
  | `Equal        (** The execution sets were equal. *)
  | `Subset       (** The final set was a proper subset of the
                      oracle set. *)
  | `Superset     (** The final set was a proper superset of the
                      oracle set. *)
  | `NoOrder      (** The two sets aren't ordered. *)
  ]
[@@deriving sexp] (* sexp_of_outcome, outcome_of_sexp *)
;;

(** [single_outcome_of herd] analyses the Herd output [herd] in
    isolation. *)
val single_outcome_of : t -> single_outcome;;

(** We can load Herd output analyses using the normal interfaces in
    [Utils.Io.LoadableIntf]. *)
include Io.LoadableIntf with type t := t
