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

open Lib
include Sim_intf

(* TODO(@MattWindsor91): roll this into, or disambiguate it from, Sim_output *)
module Result = struct
  (* TODO(@MattWindsor91): decouple this!! *)
  type t = [`Success of Sim_output.t | `Disabled | `Errored]
end

module File_map = struct
  type t = Result.t Map.M(String).t

  (* TODO(@MattWindsor91): is storing the string safe? *)
  let to_key : Fpath.t -> string = Fpath.to_string

  let make (alist : (Fpath.t, Result.t) List.Assoc.t) : t Or_error.t =
    alist
    |> Alist.map_left ~f:to_key
    |> Map.of_alist_or_error (module String)

  let get (map : t) ~(litmus_path : Fpath.t) : Result.t =
    let key = to_key litmus_path in
    let found = Map.find map key in
    Option.value found ~default:`Disabled
end

module Make (B : Common_intf.Basic) : S with type file_map := File_map.t = struct
  type run_result = Result.t

  let run_herd herd ~(input_path : Fpath.t) ~(output_path : Fpath.t) : Result.t =
    let result =
      Or_error.tag ~tag:"While running herd"
        (Herd.run_and_load_results herd ~input_path ~output_path)
    in
    match result with
    | Ok herd ->
        `Success herd
    | Error err ->
        Output.pw B.o "@[<v 4>Herd analysis error:@,%a@]@." Error.pp err ;
        `Errored

  let run_with_config config arch ~input_path ~output_path =
    match Herd.create ~config ~arch with
    | Ok herd ->
        run_herd herd ~input_path ~output_path
    | Error e ->
        Output.pw B.o "@[<v 4>Herd configuration error:@,%a@]@." Error.pp e ;
        `Errored

  let run arch ~input_path ~output_path =
    match B.herd_cfg with
    | Some h ->
        run_with_config h arch ~input_path ~output_path
    | None ->
        `Disabled

  let run_bulk_single arch (input_path : Fpath.t) ~(output_path_f : Fpath.t -> Fpath.t)
    : Fpath.t * Result.t =
    let output_path = output_path_f input_path in
    let sim = run arch ~input_path ~output_path in
    (input_path, sim)

  let run_bulk arch
      ~(input_paths : Fpath.t list) ~(output_path_f : Fpath.t -> Fpath.t)
      : File_map.t Or_error.t =
  input_paths
  |> List.map ~f:(run_bulk_single arch ~output_path_f)
  |> File_map.make
end
