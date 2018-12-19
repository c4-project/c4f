(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Core
open Utils

module Config = struct
  type t =
    { cmd        : string [@default "herd7"] [@drop_if_default]
    ; c_model    : string sexp_option
    ; asm_models : (string list, string) List.Assoc.t [@default []] [@drop_if_default]
    } [@@deriving sexp, make]
  ;;

  let create ?cmd ?c_model ?asm_models = make ?cmd ~c_model ?asm_models
end

type t =
  { config: Config.t
  }
;;

let create ~config =
  (** TODO(@MattWindsor91): validate config *)
  Or_error.return { config }
;;

type arch =
  | C
  | Assembly of string list
;;

let model_for_arch t = function
  | C -> t.config.c_model
  | Assembly emits_spec ->
    List.Assoc.find t.config.asm_models emits_spec
      ~equal:(List.equal ~equal:String.Caseless.equal)
;;

let make_argv model path =
  List.concat
    [ Option.value_map model
        ~f:(fun m -> [ "-model"; m ])
        ~default:[]
    ; [ path ]
    ]
;;

let%expect_test "make_argv: no model" =
  let argv = make_argv None "/foo/bar/herd7" in
  Sexp.output_hum Out_channel.stdout [%sexp (argv : string list)];
  [%expect {| (/foo/bar/herd7) |}]
;;

let%expect_test "make_argv: override model" =
  let argv = make_argv (Some "c11_lahav.cat") "/foo/bar/herd7" in
  Sexp.output_hum Out_channel.stdout [%sexp (argv : string list)];
  [%expect {| (-model c11_lahav.cat /foo/bar/herd7) |}]
;;

let run t arch ~path ~sink =
  let model = model_for_arch t arch in
  let prog = t.config.cmd in
  let argv = make_argv model path in
  let f _ oc = Run.Local.run ~oc ~prog argv in
  Or_error.tag ~tag:"While running herd"
    (Io.Out_sink.with_output sink ~f)
;;

let run_and_load_results t arch ~input_path ~output_path =
  let open Or_error.Let_syntax in
  let%bind () = run t arch ~path:input_path ~sink:(`File output_path) in
  Herd_output.load ~path:output_path
;;
