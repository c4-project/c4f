(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base

include Filter_intf

let lift_to_raw_strings
    ~(f : Io.In_source.t -> Io.Out_sink.t -> 'a Or_error.t)
    ~(infile : string option) ~(outfile : string option)
  : 'a Or_error.t =
  let open Or_error.Let_syntax in
  let%bind in_src  = Io.In_source.of_string_opt infile
  and      out_snk = Io.Out_sink.of_string_opt outfile
  in f in_src out_snk
;;

let lift_to_fpaths
    ~(f : Io.In_source.t -> Io.Out_sink.t -> 'a Or_error.t)
    ~(infile : Fpath.t option) ~(outfile : Fpath.t option)
  : 'a Or_error.t =
  let in_src  = Io.In_source.of_fpath_opt infile
  and out_snk = Io.Out_sink.of_fpath_opt outfile
  in f in_src out_snk
;;

module Make (B : Basic) : S with type aux = B.aux = struct
  type aux = B.aux

  let run = Io.with_input_and_output ~f:B.run

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Make_in_file_only (B : Basic_in_file_only)
  : S with type aux = B.aux = struct
  type aux = B.aux

  let run src snk =
    (* TODO(@MattWindsor91): copy stdin to temporary file? *)
    let open Or_error.Let_syntax in
    let%bind in_file = Io.In_source.to_file_err src in
    Io.Out_sink.with_output ~f:(B.run in_file) snk
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Make_files_only (B : Basic_files_only)
  : S with type aux = B.aux = struct
  type aux = B.aux

  let run src snk =
    let open Or_error.Let_syntax in
    let%bind infile  = Io.In_source.to_file_err src
    and      outfile = Io.Out_sink.to_file_err snk in
    B.run ~infile ~outfile
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain (A : S) (B : S) : S with type aux = (A.aux * B.aux) = struct
  type aux = A.aux * B.aux

  let run src snk =
    let open Or_error.Let_syntax in
    let      tmp_out      = Io.Out_sink.temp ~prefix:"act" ~ext:"tmp" in
    let%bind tmp_in       = Io.Out_sink.as_in_source tmp_out in
    let%bind a_aux = A.run src tmp_out in
    let%map  b_aux = B.run tmp_in snk in
    (a_aux, b_aux)
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain_conditional_core (B : sig
    module BCC : Basic_chain_conditional
    type aux
    val run_chained : Io.In_source.t -> Io.Out_sink.t -> aux Or_error.t
    val run_unchained : Io.In_source.t -> Io.Out_sink.t -> aux Or_error.t
  end) = struct
  type aux = B.aux

  let run src snk =
    (if B.BCC.condition src snk then B.run_chained else B.run_unchained) src snk
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain_conditional_first (B : Basic_chain_conditional)
  : S with type aux = (B.First.aux option * B.Second.aux) =
  Chain_conditional_core (struct
    module BCC = B
    type aux = B.First.aux option * B.Second.aux

    module Chained = Chain (B.First) (B.Second)

    let run_chained src snk =
      let open Or_error.Let_syntax in
      let%map (a_aux, b_aux) = Chained.run src snk in
      (Some a_aux, b_aux)
    ;;

    let run_unchained src snk =
      let open Or_error.Let_syntax in
      let%map b_aux = B.Second.run src snk in
      (None, b_aux)
    ;;
  end)
;;

module Chain_conditional_second (B : Basic_chain_conditional)
  : S with type aux = (B.First.aux * B.Second.aux option) =
  Chain_conditional_core (struct
    module BCC = B
    type aux = B.First.aux * B.Second.aux option

    module Chained = Chain (B.First) (B.Second)

    let run_chained src snk =
      let open Or_error.Let_syntax in
      let%map (a_aux, b_aux) = Chained.run src snk in
      (a_aux, Some b_aux)
    ;;

    let run_unchained src snk =
      let open Or_error.Let_syntax in
      let%map a_aux = B.First.run src snk in
      (a_aux, None)
  end)
;;
