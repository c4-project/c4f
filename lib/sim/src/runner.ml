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
include Runner_intf

module Make (B : Basic) : S = struct
  include (B : Common)

  let run (ctx : Arch.t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind () =
        Filter.run_from_fpaths ctx ~infile:(Some input_path)
          ~outfile:(Some output_path)
      in
      B.Reader.load ~path:output_path)
end

module Make_error_filter (B : Basic_error) : Basic_filter =
Act_utils.Filter.Make (struct
  type aux_i = Arch.t

  type aux_o = unit

  let name = "(errored)"

  let tmp_file_ext = Fn.const "tmp"

  let run (_ : _ Act_utils.Filter.ctx) (_ : Stdio.In_channel.t)
      (_ : Stdio.Out_channel.t) : unit Or_error.t =
    Result.Error B.error
end)

module Make_error (B : Basic_error) : S = struct
  module Filter = Make_error_filter (B)

  let name : Act_common.Id.t = Act_common.Id.of_string "error"

  let machine_id : Act_common.Id.t = Act_config.Machine.Id.default

  type t = Filter.aux_i

  let run (_ctx : t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Output.t Or_error.t =
    ignore input_path ; ignore output_path ; Result.Error B.error
end
