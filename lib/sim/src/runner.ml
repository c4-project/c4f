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

let no_make_harness
    (_arch : Arch.t) ~(input_path: Fpath.t) ~(output_dir: Fpath.t)
    : string list Or_error.t =
      ignore (input_path: Fpath.t);
      ignore (output_dir: Fpath.t);
    Or_error.error_string "This backend doesn't support harness making."

module Make (B : sig
    module Reader : Reader_intf.S
    module Unchecked_filter : Filter.S

    val make_harness_unchecked :
      Arch.t
      -> input_path:Fpath.t
      -> output_dir:Fpath.t
      -> string list Or_error.t
  end) : Runner_types.S = struct
  module Reader = B.Reader

  (* TODO(@MattWindsor91): check these! *)
  module Filter = B.Unchecked_filter
  let make_harness = B.make_harness_unchecked

  let run (arch : Arch.t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind () =
        Filter.run arch
          (Plumbing.Input.of_fpath input_path)
          (Plumbing.Output.of_fpath output_path)
      in
      B.Reader.load ~path:output_path)
end

module Make_error_reader (B : sig val error : Error.t end) : Reader_intf.S = struct
  include Plumbing.Loadable.Make (struct
    type t = Output.t

    let load_from_ic ?(path : string option) (_ic : Stdio.In_channel.t) :
        Output.t Or_error.t =
      ignore path ; Result.Error B.error

    let load_from_string (_str : string) : Output.t Or_error.t =
      Result.Error B.error
  end)

  let read_output_from_string (_s : string) : Output.t =
    Output.Errored {err= B.error}
end

module Make_error (B : sig val error : Error.t end) : Runner_types.S = struct
  module Reader = Make_error_reader (B)
  module Filter = Filter.Make_error (B)

  let make_harness = no_make_harness

  (*
  let name : Act_common.Id.t = Act_common.Id.of_string "error"

  let machine_id : Act_common.Id.t = Act_common.Id.of_string "none"
     *)

  type t = Filter.aux_i

  let run (_ctx : t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) :
      Output.t Or_error.t =
    ignore input_path ; ignore output_path ; Result.Error B.error
end
