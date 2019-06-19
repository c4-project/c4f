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

open Core_kernel
module Au = Act_utils
module Pb = Plumbing
module Ac = Act_common
module Ast = Act_c_lang.Ast

module type Basic = sig
  (** Raw AST *)
  type ast

  (** Validated AST *)
  type t

  module Frontend : Pb.Loadable_types.S with type t = ast

  val normal_tmp_file_ext : string

  val process : ast -> t Or_error.t

  val fuzz :
       ?seed:int
    -> t
    -> o:Ac.Output.t
    -> config:Act_config.Fuzz.t
    -> t Or_error.t

  val print : Out_channel.t -> t -> unit
end

type mode =
  | Print
  | Fuzz of {seed: int option; o: Ac.Output.t; config: Act_config.Fuzz.t}

module type S =
  Plumbing.Filter_types.S with type aux_i = mode and type aux_o = unit

module Make (B : Basic) : S = Pb.Filter.Make (struct
  type aux_i = mode

  type aux_o = unit

  let name = "C transformer"

  let tmp_file_ext (ctx : mode Pb.Filter_context.t) : string =
    match Pb.Filter_context.aux ctx with
    | Print ->
        B.normal_tmp_file_ext
    | Fuzz _ ->
        "c.litmus"

  let run_fuzz ?(seed : int option) (oc : Out_channel.t) (vast : B.t)
      ~(o : Ac.Output.t) ~(config : Act_config.Fuzz.t) : unit Or_error.t =
    Or_error.(vast |> B.fuzz ?seed ~o ~config >>| B.print oc)

  let run (ctx : mode Pb.Filter_context.t) ic oc : unit Or_error.t =
    let aux = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    Or_error.Let_syntax.(
      let%bind ast =
        B.Frontend.load_from_ic ~path:(Pb.Input.to_string input) ic
      in
      let%bind vast = B.process ast in
      match aux with
      | Print ->
          B.print oc vast ; Result.ok_unit
      | Fuzz {seed; o; config} ->
          run_fuzz ?seed ~o ~config oc vast)
end)

module Normal_C : S = Make (struct
  type ast = Ast.Translation_unit.t

  type t = Mini.Program.t

  let normal_tmp_file_ext = "c"

  module Frontend = Act_c_lang.Frontend.Normal

  let print oc tu =
    let f = Caml.Format.formatter_of_out_channel oc in
    let program = Mini_reify.program tu in
    Ast.Translation_unit.pp f program

  let process = Mini_convert.translation_unit

  let fuzz ?(seed : int option) (_ : t) ~(o : Ac.Output.t)
      ~(config : Act_config.Fuzz.t) : t Or_error.t =
    ignore seed ;
    ignore o ;
    ignore config ;
    Or_error.error_string "Can't fuzz a normal C file"
end)

module Litmus : S = Make (struct
  type ast = Ast.Litmus.t

  type t = Mini_litmus.Ast.Validated.t

  let normal_tmp_file_ext = "litmus"

  module Frontend = Act_c_lang.Frontend.Litmus

  let print = Mini_litmus.Pp.print

  let process = Mini_convert.litmus_of_raw_ast

  let fuzz :
         ?seed:int
      -> t
      -> o:Ac.Output.t
      -> config:Act_config.Fuzz.t
      -> t Or_error.t =
    Fuzzer.run
end)

let c_module (is_c : bool) : (module S) =
  if is_c then (module Normal_C) else (module Litmus)
