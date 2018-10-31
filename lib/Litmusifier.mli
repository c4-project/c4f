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

(** Front-end for single-file litmus conversion and explanation *)

open Core

module type S = sig
  type t =
    { o       : OutputCtx.t
    ; iname   : string
    ; inp     : In_channel.t
    ; outp    : Out_channel.t
    ; mode    : [`Explain | `Litmusify]
    ; passes  : Sanitiser_pass.Set.t
    ; symbols : string list
    }
  ;;

  val run : t -> (string, string) List.Assoc.t Or_error.t
end

module type Basic = sig
  module Frontend : LangFrontend.Intf
  module Litmus : Litmus.Intf
  module Multi_sanitiser
    : Sanitiser.S with type statement = Litmus.LS.Statement.t
                   and type sym = Litmus.LS.Symbol.t
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with type statement = Litmus.LS.Statement.t
                   and type sym = Litmus.LS.Symbol.t
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer
    : Explainer.S with type statement = Litmus.LS.Statement.t
  ;;

  val final_convert : Litmus.LS.Statement.t list -> Litmus.LS.Statement.t list

  val statements : Frontend.ast -> Litmus.LS.Statement.t list
end

module Make : functor (B : Basic) -> S
