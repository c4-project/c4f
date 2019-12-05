(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Filter_types

open struct
  module Tx = Travesty_base_exts
end

let wrap_error (name : string) : 'a Or_error.t -> 'a Or_error.t =
  Or_error.tag ~tag:(Printf.sprintf "In filter '%s'" name)

module Make (B : Basic) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run (aux : aux_i) (input : Input.t) (output : Output.t) =
    let ctx = Filter_context.make ~aux ~input ~output in
    wrap_error name
      (Io_helpers.with_input_and_output ~f:(B.run ctx) input output)
end

let copy (ic : Stdio.In_channel.t) (oc : Stdio.Out_channel.t) :
    unit Or_error.t =
  Or_error.try_with (fun () ->
      Stdio.In_channel.iter_lines ic
        ~f:(Fn.compose (Stdio.Out_channel.output_lines oc) List.return))

(** [route_input_to_file src] creates a temporary file, copies [src]'s
    contents into it, and returns it (provided that no errors occur). *)
let route_input_to_file (src : Input.t) : Fpath.t Or_error.t =
  let sink = Output.temp ~prefix:"filter" ~ext:"tmp" in
  Or_error.Let_syntax.(
    let%bind file = Output.to_fpath_err sink in
    let%map () = Io_helpers.with_input_and_output src sink ~f:copy in
    file)

(** [ensure_input_file src] returns [src] if it points to a file; otherwise,
    it creates a temporary file, copies [src]'s contents into it, and returns
    that if no errors occur. *)
let ensure_input_file (src : Input.t) : Fpath.t Or_error.t =
  match Input.to_fpath_opt src with
  | Some f ->
      Or_error.return f
  | None ->
      route_input_to_file src

(** [route_output_from_file sink ~f] creates a temporary file, passes it to
    [f], and copies the file's contents back to [sink] on successful
    completion. *)
let route_input_from_file (sink : Output.t) ~(f : Fpath.t -> 'a Or_error.t) :
    'a Or_error.t =
  let temp_out = Output.temp ~prefix:"filter" ~ext:"tmp" in
  Or_error.Let_syntax.(
    let%bind file = Output.to_fpath_err temp_out in
    let%bind result = f file in
    let temp_in = Input.file file in
    let%map () = Io_helpers.with_input_and_output temp_in sink ~f:copy in
    result)

(** [ensure_output_file sink ~f] passes [sink] to the continuation [f] if it
    points to a file; otherwise, it creates a temporary file, passes that to
    [f], and copies the file's contents back to [sink] on successful
    completion. *)
let ensure_output_file (sink : Output.t) ~(f : Fpath.t -> 'a Or_error.t) :
    'a Or_error.t =
  match Output.to_fpath_opt sink with
  | Some file ->
      f file
  | None ->
      route_input_from_file sink ~f

module Make_in_file_only (B : Basic_in_file_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run (aux : aux_i) (input : Input.t) (output : Output.t) =
    let ctx = Filter_context.make ~aux ~input ~output in
    wrap_error name
      Or_error.Let_syntax.(
        let%bind in_file = ensure_input_file input in
        Output.with_output output ~f:(B.run ctx in_file))
end

module Make_files_only (B : Basic_files_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run (aux : aux_i) (input : Input.t) (output : Output.t) =
    let ctx = Filter_context.make ~aux ~input ~output in
    wrap_error name
      Or_error.Let_syntax.(
        let%bind infile = ensure_input_file input in
        ensure_output_file output ~f:(fun outfile ->
            B.run ctx ~infile ~outfile))
end

module Adapt (B : Basic_adapt) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i

  type aux_o = B.aux_o

  let name = B.Original.name

  let run (new_i : aux_i) (src : Input.t) (sink : Output.t) :
      aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%bind old_i = B.adapt_i new_i in
      let%bind old_o = B.Original.run old_i src sink in
      B.adapt_o old_o)
end

module Make_on_runner (R : Basic_on_runner) :
  S with type aux_i = R.aux_i and type aux_o = unit =
Make_in_file_only (struct
  let make_argv (aux : R.aux_i)
      ({input; _} : Copy_projection.t Copy_spec.Pair.t) :
      string list Or_error.t =
    Or_error.(
      input |> Copy_projection.all_remote |> Copy_spec.get_file
      >>= R.argv aux)

  include R

  type aux_o = unit

  let run (ctx : aux_i Filter_context.t) (infile : Fpath.t)
      (oc : Stdio.Out_channel.t) : unit Or_error.t =
    let aux = Filter_context.aux ctx in
    let prog = R.prog aux in
    R.Runner.run_with_copy ~oc ~prog
      {input= Copy_spec.file infile; output= Copy_spec.nothing}
      ~argv_f:(make_argv aux)
end)
