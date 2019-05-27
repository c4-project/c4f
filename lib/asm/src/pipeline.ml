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

open Base
module Ac = Act_common
module Pb = Plumbing

module Input = struct
  type 'job t =
    { file_type: Ac.File_type.t
    ; job_input:
           Act_c.Filters.Output.t Pb.Chain_context.t
        -> 'job Job.t Act_config.Compiler.Chain_input.t }
  [@@deriving make, fields]
end

module Output = struct
  type t =
    {c_output: Act_c.Filters.Output.t option; job_output: Job.Output.t}
  [@@deriving make, fields]
end

module type S = sig
  type cfg

  include
    Pb.Filter_types.S
    with type aux_i = cfg Input.t
     and type aux_o = Output.t
end

module type Basic = sig
  type cfg

  include
    Pb.Filter_types.S
    with type aux_i = cfg Job.t Act_config.Compiler.Chain_input.t
     and type aux_o = Job.Output.t
end

module Make (J : Basic) : S with type cfg = J.cfg = struct
  type cfg = J.cfg

  include Pb.Filter_chain.Make_conditional_first (struct
    module First = Act_c.Filters.Litmus
    module Second = J

    type aux_i = J.cfg Input.t

    type aux_o = Output.t

    let combine_output (c_output : Act_c.Filters.Output.t option)
        (job_output : Job.Output.t) : Output.t =
      Output.make ?c_output ~job_output ()

    let select (ctx : J.cfg Input.t Pb.Filter_context.t) :
        [ `Both of
          First.aux_i * (First.aux_o Pb.Chain_context.t -> Second.aux_i)
        | `One of First.aux_o Pb.Chain_context.t -> Second.aux_i ] =
      let aux = Pb.Filter_context.aux ctx in
      let file_type = Input.file_type aux in
      let rest = Input.job_input aux in
      let input = Pb.Filter_context.input ctx in
      if Act_common.File_type.is_c_litmus input file_type then
        `Both (Act_c.Filters.Delitmus, rest)
      else `One rest
  end)
end

let make (type c spec)
    (module Resolver : Act_config.Compiler.S_resolver with type spec = spec)
    (module Runner : Runner_intf.S with type cfg = c) (target : spec) :
    (module S with type cfg = c) Or_error.t =
  Or_error.Let_syntax.(
    let%map (module Chained_runner) =
      Resolver.chained_filter_from_spec target (module Runner)
    in
    let module B = struct
      type cfg = c

      include Chained_runner
    end in
    (module Make (B) : S with type cfg = c))
