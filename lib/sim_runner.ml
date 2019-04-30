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
open Utils

include Sim_runner_intf

module Make (B : Basic) : S with type t = B.Filter.aux_i = struct
  type t = B.Filter.aux_i

  let run_on_paths (ctx : t) ~(input_path : Fpath.t)
      ~(output_path : Fpath.t) : unit Or_error.t =
    B.Filter.run ctx (Io.In_source.of_fpath input_path) (Io.Out_sink.file output_path)


  let run_and_load_results (ctx : t) ~(input_path : Fpath.t)
      ~(output_path : Fpath.t) : Sim_output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind () = run_on_paths ctx ~input_path ~output_path in
      B.Reader.load ~path:output_path
    )
end
