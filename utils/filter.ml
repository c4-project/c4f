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
    ~(f : 'i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
    (aux_in : 'i)
    ~(infile : string option)
    ~(outfile : string option)
  : 'o Or_error.t =
  let open Or_error.Let_syntax in
  let%bind in_src  = Io.In_source.of_string_opt infile
  and      out_snk = Io.Out_sink.of_string_opt outfile
  in f aux_in in_src out_snk
;;

let lift_to_fpaths
    ~(f : 'i -> Io.In_source.t -> Io.Out_sink.t -> 'o Or_error.t)
    (aux_in : 'i)
    ~(infile : Fpath.t option)
    ~(outfile : Fpath.t option)
  : 'o Or_error.t =
  let in_src  = Io.In_source.of_fpath_opt infile
  and out_snk = Io.Out_sink.of_fpath_opt outfile
  in f aux_in in_src out_snk
;;

module Make (B : Basic) : S with type aux_i = B.aux_i
                             and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i
  type aux_o = B.aux_o

  let run input = Io.with_input_and_output ~f:(B.run input)

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Make_in_file_only (B : Basic_in_file_only)
  : S with type aux_i = B.aux_i
       and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i
  type aux_o = B.aux_o

  let run input src snk =
    (* TODO(@MattWindsor91): copy stdin to temporary file? *)
    let open Or_error.Let_syntax in
    let%bind in_file = Io.In_source.to_file_err src in
    Io.Out_sink.with_output ~f:(B.run input in_file) snk
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Make_files_only (B : Basic_files_only)
  : S with type aux_i = B.aux_i
       and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i
  type aux_o = B.aux_o

  let run input src snk =
    let open Or_error.Let_syntax in
    let%bind infile  = Io.In_source.to_file_err src
    and      outfile = Io.Out_sink.to_file_err snk in
    B.run input ~infile ~outfile
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain (A : S) (B : S)
  : S with type aux_i = (A.aux_i * B.aux_i)
       and type aux_o = (A.aux_o * B.aux_o) = struct
  type aux_i = A.aux_i * B.aux_i
  type aux_o = A.aux_o * B.aux_o

  let run (a_input, b_input) src snk =
    let open Or_error.Let_syntax in
    let      tmp_out      = Io.Out_sink.temp ~prefix:"act" ~ext:"tmp" in
    let%bind tmp_in       = Io.Out_sink.as_in_source tmp_out in
    let%bind a_output = A.run a_input src tmp_out in
    let%map  b_output = B.run b_input tmp_in snk in
    (a_output, b_output)
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain_conditional_core (B : sig
    module BCC : Basic_chain_conditional
    type aux_o
    val run_chained
      : BCC.First.aux_i -> BCC.Second.aux_i -> Io.In_source.t -> Io.Out_sink.t -> aux_o Or_error.t
    val run_unchained
      : BCC.aux_i_single -> Io.In_source.t -> Io.Out_sink.t -> aux_o Or_error.t
  end) = struct
  type aux_i = B.BCC.aux_i_combi
  type aux_o = B.aux_o

  let run (aux_in : aux_i) src snk =
    ( match B.BCC.select aux_in src snk with
      | `Both (a_in, b_in) -> B.run_chained a_in b_in
      | `One  aux_in'      -> B.run_unchained aux_in'
    ) src snk
  ;;

  let run_from_string_paths = lift_to_raw_strings ~f:run
  let run_from_fpaths       = lift_to_fpaths ~f:run
end

module Chain_conditional_first (B : Basic_chain_conditional_first)
  : S with type aux_i = B.aux_i_combi
       and type aux_o = (B.First.aux_o option * B.Second.aux_o) =
  Chain_conditional_core (struct
    module BCC = struct
      include B
      type aux_i_single = B.Second.aux_i
    end
    type aux_o = (B.First.aux_o option * B.Second.aux_o)

    module Chained = Chain (B.First) (B.Second)

    let run_chained a_in b_in src snk =
      let open Or_error.Let_syntax in
      let%map (a_out, b_out) = Chained.run (a_in, b_in) src snk in
      (Some a_out, b_out)
    ;;

    let run_unchained b_in src snk =
      let open Or_error.Let_syntax in
      let%map b_out = B.Second.run b_in src snk in
      (None, b_out)
    ;;
  end)
;;

module Chain_conditional_second (B : Basic_chain_conditional_second)
  : S with type aux_i = B.aux_i_combi
       and type aux_o = (B.First.aux_o * B.Second.aux_o option) =
  Chain_conditional_core (struct
    module BCC = struct
      include B
      type aux_i_single = B.First.aux_i
    end
    type aux_o = (B.First.aux_o * B.Second.aux_o option)

    module Chained = Chain (B.First) (B.Second)

    let run_chained a_in b_in src snk =
      let open Or_error.Let_syntax in
      let%map (a_out, b_out) = Chained.run (a_in, b_in) src snk in
      (a_out, Some b_out)
    ;;

    let run_unchained a_in src snk =
      let open Or_error.Let_syntax in
      let%map a_out = B.First.run a_in src snk in
      (a_out, None)
  end)
;;
