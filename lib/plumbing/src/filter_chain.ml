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
open Filter_chain_types

module Make (B : Basic_unconditional) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o = struct
  type aux_i = B.aux_i

  type aux_o = B.aux_o

  let name = Printf.sprintf "(%s | %s)" B.First.name B.Second.name

  let check_second_input (aux : B.aux_i) : B.Second.aux_i =
    B.second_input aux Chain_context.Checking_ahead

  let tmp_file_ext (ctx : B.aux_i Filter_context.t) : string =
    let ctx' = Filter_context.On_aux.map ~f:check_second_input ctx in
    B.Second.tmp_file_ext ctx'

  let make_temp (a_ctx : B.First.aux_i Filter_context.t) :
      (Output.t * Input.t) Or_error.t =
    let tmp_ext = B.First.tmp_file_ext a_ctx in
    let tmp_out = Output.temp ~prefix:"act" ~ext:tmp_ext in
    Or_error.Let_syntax.(
      let%map tmp_in = Output.as_input tmp_out in
      (tmp_out, tmp_in))

  let run (aux : B.aux_i) (input : Input.t) (output : Output.t) =
    let a_aux = B.first_input aux in
    let a_ctx = Filter_context.make ~aux:a_aux ~input ~output in
    Or_error.Let_syntax.(
      let%bind a_out, b_in = make_temp a_ctx in
      let%bind a_output = B.First.run a_aux input a_out in
      let b_aux = B.second_input aux (Ran a_output) in
      let%map b_output = B.Second.run b_aux b_in output in
      B.combine_output a_output b_output)
end

module Make_tuple (First : Filter_types.S) (Second : Filter_types.S) :
  Filter_types.S
  with type aux_i =
              First.aux_i * (First.aux_o Chain_context.t -> Second.aux_i)
   and type aux_o = First.aux_o * Second.aux_o = Make (struct
  type aux_i = First.aux_i * (First.aux_o Chain_context.t -> Second.aux_i)

  type aux_o = First.aux_o * Second.aux_o

  module First = First
  module Second = Second

  let first_input : aux_i -> First.aux_i = fst

  let second_input : aux_i -> First.aux_o Chain_context.t -> Second.aux_i =
    snd

  let combine_output (f : First.aux_o) (s : Second.aux_o) : aux_o = (f, s)
end)

module type Chain_conditional_variables = sig
  module BCC : Basic_conditional

  type aux_o

  val name : string

  val tmp_file_ext : BCC.aux_i Filter_context.t -> string

  val run_chained :
       BCC.First.aux_i
    -> (BCC.First.aux_o Chain_context.t -> BCC.Second.aux_i)
    -> Input.t
    -> Output.t
    -> aux_o Or_error.t

  val run_unchained :
    BCC.aux_i_single -> Input.t -> Output.t -> aux_o Or_error.t
end

module Make_conditional_core (B : Chain_conditional_variables) = struct
  type aux_i = B.BCC.aux_i

  type aux_o = B.aux_o

  let name = B.name

  let tmp_file_ext = B.tmp_file_ext

  let run (aux : aux_i) (input : Input.t) (output : Output.t) =
    let ctx = Filter_context.make ~aux ~input ~output in
    ( match B.BCC.select ctx with
    | `Both (a_in, b_in) ->
        B.run_chained a_in b_in
    | `One aux_in' ->
        B.run_unchained aux_in' )
      input output
end

module Make_conditional_first (B : Basic_conditional_first) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o =
Make_conditional_core (struct
  module BCC = struct
    include B

    type aux_i_single = B.First.aux_o Chain_context.t -> B.Second.aux_i

    type 'a first_o_wrapper = 'a option

    type 'a second_o_wrapper = 'a
  end

  type aux_o = B.aux_o

  module Chained = Make_tuple (B.First) (B.Second)

  let name = Printf.sprintf "(%s? | %s)" B.First.name B.Second.name

  (* Temporary file output depends on the _second_ filter, which is always
     running. *)
  let tmp_file_ext (ctx : B.aux_i Filter_context.t) : string =
    match BCC.select ctx with
    | `Both (_, aux) | `One aux ->
        (* TODO(@MattWindsor91): unify with 'check_second_input' above. *)
        let ctx' =
          Filter_context.On_aux.map ~f:(fun _ -> aux Checking_ahead) ctx
        in
        B.Second.tmp_file_ext ctx'

  let run_chained (a_in : B.First.aux_i)
      (b_in : B.First.aux_o Chain_context.t -> B.Second.aux_i)
      (input : Input.t) (output : Output.t) : B.aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map a_out, b_out = Chained.run (a_in, b_in) input output in
      B.combine_output (Some a_out) b_out)

  let run_unchained
      (b_in_f : B.First.aux_o Chain_context.t -> B.Second.aux_i)
      (input : Input.t) (output : Output.t) : B.aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map b_out = B.Second.run (b_in_f Skipped) input output in
      B.combine_output None b_out)
end)

module Make_conditional_second (B : Basic_conditional_second) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o =
Make_conditional_core (struct
  module BCC = struct
    include B

    type aux_i_single = B.First.aux_i

    type 'a first_o_wrapper = 'a

    type 'a second_o_wrapper = 'a option
  end

  type aux_o = B.aux_o

  module Chained = Make_tuple (B.First) (B.Second)

  let name = Printf.sprintf "(%s | %s?)" B.First.name B.Second.name

  (* Temporary file output depends on the _second_ filter, which may or may
     not be running. *)
  let tmp_file_ext (ctx : B.aux_i Filter_context.t) : string =
    match BCC.select ctx with
    | `Both (_, aux) ->
        (* TODO(@MattWindsor91): unify with 'check_second_input' above. *)
        let ctx' =
          Filter_context.On_aux.map ~f:(fun _ -> aux Checking_ahead) ctx
        in
        B.Second.tmp_file_ext ctx'
    | `One aux ->
        B.First.tmp_file_ext
          (Filter_context.On_aux.map ~f:(fun _ -> aux) ctx)

  let run_chained (a_in : B.First.aux_i)
      (b_in : B.First.aux_o Chain_context.t -> B.Second.aux_i)
      (input : Input.t) (output : Output.t) : B.aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map a_out, b_out = Chained.run (a_in, b_in) input output in
      B.combine_output a_out (Some b_out))

  let run_unchained (a_in : B.First.aux_i) (input : Input.t)
      (output : Output.t) : B.aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map a_out = B.First.run a_in input output in
      B.combine_output a_out None)
end)
