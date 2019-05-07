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
open Travesty_base_exts

include Bulk_intf

module File_map = struct
  type t = Output.t Map.M(String).t
  let to_key : Fpath.t -> string = Fpath.to_string

  let make (alist : (Fpath.t, Output.t) List.Assoc.t) : t Or_error.t =
    alist
    |> Alist.map_left ~f:to_key
    |> Map.of_alist_or_error (module String)

  let get (map : t) ~(litmus_path : Fpath.t) : Output.t =
    let key = to_key litmus_path in
    let found = Map.find map key in
    Option.value found ~default:(Output.not_found litmus_path)
end

module Make (R : Runner.S) : S with type file_map := File_map.t = struct
  module Job = struct
    type t = { input_paths : Fpath.t list
             ; output_path_f : Fpath.t -> Fpath.t
             ; arch : Arch.t
             }
  end

  let run_with_output_path (arch : Arch.t) ~(input_path : Fpath.t) ~(output_path : Fpath.t) : Output.t =
    Or_error.(
      R.run_and_load_results arch ~input_path ~output_path
      |> tag ~tag:"While running herd"
      |> Output.join
    )

  let run_single (job : Job.t) (input_path : Fpath.t)
    : Fpath.t * Output.t =
    let output_path = job.output_path_f input_path in
    let sim = run_with_output_path job.arch ~input_path ~output_path in
    (input_path, sim)

  let run (job : Job.t) : File_map.t Or_error.t =
  job.input_paths
  |> List.map ~f:(run_single job)
  |> File_map.make
end
