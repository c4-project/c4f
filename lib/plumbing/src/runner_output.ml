(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

type t = Nowhere | To_out_channel of Out_channel.t | To_buffer of Buffer.t

let stdout : t = To_out_channel stdout

let to_stdoutf : t -> (Bytes.t -> int -> unit) Staged.t = function
  | Nowhere ->
      Staged.stage (fun _ _ -> ())
  | To_out_channel o ->
      Staged.stage (fun buf len -> Out_channel.output o ~buf ~pos:0 ~len)
  | To_buffer b ->
      Staged.stage (fun buf len -> Buffer.add_subbytes b buf ~pos:0 ~len)

let output_lines_to_buffer (b : Buffer.t) (xs : string list) : unit =
  xs |> List.intersperse ~sep:"\n" |> List.iter ~f:(Buffer.add_string b)

let output_lines : t -> string list -> unit = function
  | Nowhere ->
      Fn.const ()
  | To_out_channel o ->
      Out_channel.output_lines o
  | To_buffer b ->
      output_lines_to_buffer b
