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

module type Intf = sig
  type t =
    { o      : OutputCtx.t
    ; cid    : CompilerSpec.Id.t
    ; spec   : CompilerSpec.t
    ; iname  : string
    ; inp    : In_channel.t
    ; outp   : Out_channel.t
    ; mode   : [`Explain | `Litmusify]
    ; passes : Sanitiser.Pass.Set.t
    }
  ;;

  val run : t -> unit Or_error.t
end

module type S = sig
  type statement

  module Frontend  : LangFrontend.Intf
  module Litmus    : Litmus.Intf with type LS.Statement.t = statement
  module Sanitiser : Sanitiser.Intf with type statement = statement
  module Explainer : Explainer.S with type statement = statement

  val final_convert : statement list -> statement list

  val statements : Frontend.ast -> statement list
end

module Make : functor (M : S) -> Intf
