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
include Filter_intf

let lift_to_raw_strings
    ~(f : 'i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
    (aux_in : 'i) ~(infile : string option) ~(outfile : string option) :
    'o Or_error.t =
  let open Or_error.Let_syntax in
  let%bind in_src = Io.In_source.of_string_opt infile
  and out_snk = Io.Out_sink.of_string_opt outfile in
  f aux_in in_src out_snk

let lift_to_fpaths
    ~(f : 'i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
    (aux_in : 'i) ~(infile : Fpath.t option) ~(outfile : Fpath.t option) :
    'o Or_error.t =
  let in_src = Io.In_source.of_fpath_opt infile
  and out_snk = Io.Out_sink.of_fpath_opt outfile in
  f aux_in in_src out_snk

module Make (B : Basic) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run aux src snk =
    Or_error.tag
      ~tag:(Printf.sprintf "In filter '%s'" name)
      (Io.with_input_and_output
         ~f:(fun src ic sink -> B.run {aux; src; sink} ic)
         src snk)

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

let copy (_src : Io.In_source.t) (ic : Stdio.In_channel.t)
    (_snk : Io.Out_sink.t) (oc : Stdio.Out_channel.t) : unit Or_error.t =
  Or_error.try_with (fun () ->
      Stdio.In_channel.iter_lines ic
        ~f:(Fn.compose (Stdio.Out_channel.output_lines oc) List.return) )

(** [route_input_to_file src] creates a temporary file, copies [src]'s
    contents into it, and returns it (provided that no errors occur). *)
let route_input_to_file (src : Io.In_source.t) : Fpath.t Or_error.t =
  let open Or_error.Let_syntax in
  let sink = Io.Out_sink.temp ~prefix:"filter" ~ext:"tmp" in
  let%bind file = Io.Out_sink.to_file_err sink in
  let%map () = Io.with_input_and_output src sink ~f:copy in
  file

(** [ensure_input_file src] returns [src] if it points to a file; otherwise,
    it creates a temporary file, copies [src]'s contents into it, and
    returns that if no errors occur. *)
let ensure_input_file (src : Io.In_source.t) : Fpath.t Or_error.t =
  match Io.In_source.to_file src with
  | Some f ->
      Or_error.return f
  | None ->
      route_input_to_file src

(** [route_output_from_file sink ~f] creates a temporary file, passes it to
    [f], and copies the file's contents back to [sink] on successful
    completion. *)
let route_input_from_file (sink : Io.Out_sink.t)
    ~(f : Fpath.t -> 'a Or_error.t) : 'a Or_error.t =
  let open Or_error.Let_syntax in
  let temp_out = Io.Out_sink.temp ~prefix:"filter" ~ext:"tmp" in
  let%bind file = Io.Out_sink.to_file_err temp_out in
  let%bind result = f file in
  let temp_in = Io.In_source.file file in
  let%map () = Io.with_input_and_output temp_in sink ~f:copy in
  result

(** [ensure_output_file sink ~f] passes [sink] to the continuation [f] if it
    points to a file; otherwise, it creates a temporary file, passes that to
    [f], and copies the file's contents back to [sink] on successful
    completion. *)
let ensure_output_file (sink : Io.Out_sink.t)
    ~(f : Fpath.t -> 'a Or_error.t) : 'a Or_error.t =
  match Io.Out_sink.to_file sink with
  | Some file ->
      f file
  | None ->
      route_input_from_file sink ~f

module Make_in_file_only (B : Basic_in_file_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run aux src sink =
    let open Or_error.Let_syntax in
    Or_error.tag
      ~tag:(Printf.sprintf "In filter '%s'" name)
      (let%bind in_file = ensure_input_file src in
       Io.Out_sink.with_output sink ~f:(fun _ ->
           B.run {aux; src; sink} in_file ))

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

module Make_files_only (B : Basic_files_only) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  include B

  let run aux src sink =
    Or_error.tag
      ~tag:(Printf.sprintf "In filter '%s'" name)
      (let open Or_error.Let_syntax in
      let%bind infile = ensure_input_file src in
      ensure_output_file sink ~f:(fun outfile ->
          B.run {aux; src; sink} ~infile ~outfile ))

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

module Chain (B : Basic_chain_unconditional) :
  S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o * B.Second.aux_o = struct
  type aux_i = B.aux_i

  type aux_o = B.First.aux_o * B.Second.aux_o

  let name = Printf.sprintf "(%s | %s)" B.First.name B.Second.name

  let tmp_file_ext {aux; src; sink} =
    let snd_aux = B.second_input aux `Checking_ahead in
    B.Second.tmp_file_ext {aux= snd_aux; src; sink}

  let make_temp a_ctx =
    let open Or_error.Let_syntax in
    let tmp_ext = B.First.tmp_file_ext a_ctx in
    let tmp_out = Io.Out_sink.temp ~prefix:"act" ~ext:tmp_ext in
    let%map tmp_in = Io.Out_sink.as_in_source tmp_out in
    (tmp_out, tmp_in)

  let run aux src sink =
    let a_aux = B.first_input aux in
    let a_ctx = {aux= a_aux; src; sink} in
    let open Or_error.Let_syntax in
    let%bind a_out, b_in = make_temp a_ctx in
    let%bind a_output = B.First.run a_aux src a_out in
    let b_aux = B.second_input aux (`Ran a_output) in
    let%map b_output = B.Second.run b_aux b_in sink in
    (a_output, b_output)

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

module Chain_tuple (First : S) (Second : S) :
  S
  with type aux_i = First.aux_i * (First.aux_o chain_output -> Second.aux_i)
   and type aux_o = First.aux_o * Second.aux_o = Chain (struct
  module First = First
  module Second = Second

  type aux_i = First.aux_i * (First.aux_o chain_output -> Second.aux_i)

  let first_input = fst

  let second_input = snd
end)

module type Chain_conditional_variables = sig
  module BCC : Basic_chain_conditional

  type aux_o

  val name : string

  val tmp_file_ext : BCC.aux_i ctx -> string

  val run_chained :
       BCC.First.aux_i
    -> (BCC.First.aux_o chain_output -> BCC.Second.aux_i)
    -> Io.In_source.t
    -> Io.Out_sink.t
    -> aux_o Or_error.t

  val run_unchained :
    BCC.aux_i_single -> Io.In_source.t -> Io.Out_sink.t -> aux_o Or_error.t
end

module Chain_conditional_core (B : Chain_conditional_variables) = struct
  type aux_i = B.BCC.aux_i

  type aux_o = B.aux_o

  let name = B.name

  let tmp_file_ext = B.tmp_file_ext

  let run (aux : aux_i) src sink =
    ( match B.BCC.select {aux; src; sink} with
    | `Both (a_in, b_in) ->
        B.run_chained a_in b_in
    | `One aux_in' ->
        B.run_unchained aux_in' )
      src sink

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

module Chain_conditional_first (B : Basic_chain_conditional_first) :
  S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o option * B.Second.aux_o =
Chain_conditional_core (struct
  module BCC = struct
    include B

    type aux_i_single = B.First.aux_o chain_output -> B.Second.aux_i
  end

  type aux_o = B.First.aux_o option * B.Second.aux_o

  module Chained = Chain_tuple (B.First) (B.Second)

  let name = Printf.sprintf "(%s? | %s)" B.First.name B.Second.name

  (* Temporary file output depends on the _second_ filter, which is always
     running. *)
  let tmp_file_ext ({src; sink; _} as ctx) : string =
    match BCC.select ctx with
    | `Both (_, aux) | `One aux ->
        B.Second.tmp_file_ext {aux= aux `Checking_ahead; src; sink}

  let run_chained a_in b_in src snk =
    let open Or_error.Let_syntax in
    let%map a_out, b_out = Chained.run (a_in, b_in) src snk in
    (Some a_out, b_out)

  let run_unchained b_in_f src snk =
    let open Or_error.Let_syntax in
    let%map b_out = B.Second.run (b_in_f `Skipped) src snk in
    (None, b_out)
end)

module Chain_conditional_second (B : Basic_chain_conditional_second) :
  S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o * B.Second.aux_o option =
Chain_conditional_core (struct
  module BCC = struct
    include B

    type aux_i_single = B.First.aux_i
  end

  type aux_o = B.First.aux_o * B.Second.aux_o option

  module Chained = Chain_tuple (B.First) (B.Second)

  let name = Printf.sprintf "(%s | %s?)" B.First.name B.Second.name

  (* Temporary file output depends on the _second_ filter, which may or may
     not be running. *)
  let tmp_file_ext ({src; sink; _} as ctx) : string =
    match BCC.select ctx with
    | `Both (_, aux) ->
        B.Second.tmp_file_ext {aux= aux `Checking_ahead; src; sink}
    | `One aux ->
        B.First.tmp_file_ext {aux; src; sink}

  let run_chained a_in b_in src snk =
    let open Or_error.Let_syntax in
    let%map a_out, b_out = Chained.run (a_in, b_in) src snk in
    (a_out, Some b_out)

  let run_unchained a_in src snk =
    let open Or_error.Let_syntax in
    let%map a_out = B.First.run a_in src snk in
    (a_out, None)
end)

module Adapt (B : Basic_adapt) :
  S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i

  type aux_o = B.aux_o

  let name = B.Original.name

  let tmp_file_ext {aux= new_i; src; sink} : string =
    match B.adapt_i new_i with
    | Result.Ok aux ->
        B.Original.tmp_file_ext {aux; src; sink}
    | Result.Error _ ->
        "tmp"

  let run (new_i : aux_i) (src : Io.In_source.t) (sink : Io.Out_sink.t) :
      aux_o Or_error.t =
    let open Or_error.Let_syntax in
    let%bind old_i = B.adapt_i new_i in
    let%bind old_o = B.Original.run old_i src sink in
    B.adapt_o old_o

  let run_from_string_paths = lift_to_raw_strings ~f:run

  let run_from_fpaths = lift_to_fpaths ~f:run
end

module Make_on_runner (R : Basic_on_runner) :
  S with type aux_i = R.aux_i and type aux_o = unit =
Make_in_file_only (struct
  let get_file : string Copy_spec.t -> string Or_error.t = function
    | Files fs ->
        List.one fs
    | Directory _ ->
        Or_error.error_string "Expected one file; got directory"
    | Nothing ->
        Or_error.error_string "Expected one file; got nothing"

  let make_argv (aux : R.aux_i) ~(input : string Copy_spec.t)
      ~(output : string Copy_spec.t) : string list Or_error.t =
    ignore output ;
    let open Or_error.Let_syntax in
    let%map file = get_file input in
    R.argv aux file

  include R

  type aux_o = unit

  let run ({aux; _} : aux_i ctx) (infile : Fpath.t)
      (oc : Stdio.Out_channel.t) : unit Or_error.t =
    let prog = R.prog aux in
    R.Runner.run_with_copy ~oc ~prog
      {input= Copy_spec.file infile; output= Copy_spec.nothing}
      (make_argv aux)
end)
