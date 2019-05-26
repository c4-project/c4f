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
module Tx = Travesty_base_exts
open Stub_gen_intf

module Config = struct
  type t = {separator: string option} [@@deriving fields, make]

  let default = make ()
end

module type S = S with type config := Config.t

module type S_filter = Runner_intf.S with type cfg = Config.t

(* TODO(@MattWindsor91): don't print separator after the last element. *)
let print_separator (f : Formatter.t) (config : Config.t) : unit =
  Option.iter (Config.separator config) ~f:(Fmt.pf f "@[%s@]@.")

module Make (B : Basic) : S with module Lang = B.Src_lang = struct
  module Lang = B.Src_lang
  module San = Act_sanitiser.Instance.Make_multi (B.Sanitiser_hook)

  let gen_and_dump_asm_stub (i : int) (program : San.Output.Program.t)
      ~(oc : Stdio.Out_channel.t) ~(config : Config.t) : unit Or_error.t =
    let f = Caml.Format.formatter_of_out_channel oc in
    if Int.(i <> 0) then print_separator f config ;
    let listing = San.Output.Program.listing program in
    Or_error.Let_syntax.(
      let%map stub = B.as_asm_stub listing in
      Fmt.pf f "@[<v>%a@]@." Act_c.Asm_stub.pp stub)

  let gen_and_dump_asm_stubs (programs : San.Output.Program.t list)
      ~(oc : Stdio.Out_channel.t) ~(config : Config.t) : unit Or_error.t =
    programs
    |> List.mapi ~f:(gen_and_dump_asm_stub ~oc ~config)
    |> Or_error.combine_errors_unit

  let run_stub_gen (oc : Stdio.Out_channel.t) ~(in_name : string)
      ~(program : Lang.Program.t) ~(symbols : Lang.Symbol.t list)
      ~(config : Config.t) ~(passes : Set.M(Act_sanitiser.Pass_group).t) :
      Job.Output.t Or_error.t =
    ignore config ;
    Or_error.Let_syntax.(
      let%bind san = San.sanitise ~passes ~symbols program in
      let programs = San.Output.programs san in
      let%map () = gen_and_dump_asm_stubs programs ~oc ~config in
      let redirects_raw = San.Output.redirects san in
      let redirects = Lang.Symbol.R_map.to_string_alist redirects_raw in
      Job.Output.make (Fmt.always "?") in_name redirects [])

  module Filter : S_filter = Runner.Make (struct
    type cfg = Config.t

    module Symbol = B.Src_lang.Symbol
    module Program = B.Program

    let name = "Stub generator"

    let tmp_file_ext = "txt"

    let default_config () = Config.default

    let run = run_stub_gen
  end)
end
