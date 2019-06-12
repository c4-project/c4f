(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Litmusifier: auxiliary field generation

    The functions in this module assist with building the 'auxiliary' fields
    of an assembly Litmus test: the init block, the locations stanza, and
    the postcondition. *)

open Base

(** {2 Making individual auxiliary fields}

    We mainly expose these functions for testing, but they could be useful
    in isolation. *)

val make_init :
     ?c_variables:Act_common.C_variables.Map.t
  -> heap_symbols:Act_abstract.Symbol.Set.t
  -> of_int:(int -> 'const)
  -> unit
  -> (Act_common.C_id.t, 'const) List.Assoc.t
(** [make_init ?c_variables ~heap_symbols ~of_int ()] makes an init block
    either by directly adapting the information in [c_variables] using
    [of_int], if [c_variables] exists, or by assigning each heap symbol in
    [heap_syms] to [of_int 0]. *)

val make_locations :
     ?postcondition:'const Act_litmus.Ast_base.Postcondition.t
  -> ?c_variables:Act_common.C_variables.Map.t
  -> init:(Act_common.C_id.t, _) List.Assoc.t
  -> unit
  -> Act_common.C_id.t list
(** [make_locations ?postcondition ?c_variables ~init ()] makes a
    'locations' stanza, either from the variables in [c_variables] (if
    present), or by or just by taking the LHS of [init]. It adds any global
    variables mentioned in the postcondition, if given. *)

val is_live_symbol :
  Act_common.C_id.t -> heap_symbols:Act_abstract.Symbol.Set.t -> bool
(** [is_live_symbol cid ~heap_symbols] checks whether C identifier [cid]
    refers to a symbol in [heap_symbols]. It doesn't do any (de-)mangling,
    so this should be done beforehand. *)

val live_symbols_only :
     Act_common.C_variables.Map.t
  -> heap_symbols:Act_abstract.Symbol.Set.t
  -> Act_common.C_variables.Map.t
(** [live_symbols_only cvars ~heap_symbols] restricts [cvars] to entries
    whose variable ID is a live symbol according to
    [is_live_symbol ~heap_symbols]. *)

(** {2 Making auxiliary fields in one go} *)

val make :
     ?c_variables:Act_common.C_variables.Map.t
  -> ?postcondition:'const Act_litmus.Ast_base.Postcondition.t
  -> heap_symbols:Act_abstract.Symbol.Set.t
  -> of_int:(int -> 'const)
  -> 'const Act_litmus.Aux.t
(** [make ?c_variables ?postcondition ~heap_symbols ~of_int] makes a record
    of Litmus auxiliary fields by processing the optional variable map
    [c_variables] against the heap symbol set [heap_symbols], adding the
    postcondition [postcondition] verbatim, and converting initial values to
    language constants using [of_int]. *)
