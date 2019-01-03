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

type arch =
  | C
  | Assembly of string list
;;

type t =
  { config: Config.t
  ; arch  : arch
  }
  [@@deriving make]
;;

let create ~config ~arch =
  (** TODO(@MattWindsor91): validate config *)
  Or_error.return (make ~config ~arch)
;;

let model_for_arch { config; arch } = match arch with
  | C -> config.c_model
  | Assembly emits_spec ->
    List.Assoc.find config.asm_models emits_spec
      ~equal:(List.equal ~equal:String.Caseless.equal)
;;

let make_argv model (path : Fpath.t) =
  List.concat
    [ Option.value_map model
        ~f:(fun m -> [ "-model"; m ])
        ~default:[]
    ; [ Fpath.to_string path ]
    ]
;;

let%expect_test "make_argv: no model" =
  let argv = make_argv None (Fpath.v "herd7") in
  Sexp.output_hum Out_channel.stdout [%sexp (argv : string list)];
  [%expect {| (herd7) |}]
;;

let%expect_test "make_argv: override model" =
  let argv = make_argv (Some "c11_lahav.cat") (Fpath.v "herd7") in
  Sexp.output_hum Out_channel.stdout [%sexp (argv : string list)];
  [%expect {| (-model c11_lahav.cat herd7) |}]
;;

module Filter : Filter.S with type aux_i = t
                          and type aux_o = unit =
  Filter.Make_in_file_only (struct
    type aux_i = t
    type aux_o = unit

    let run (ctx : t) (path : Fpath.t)
        (_sink : Io.Out_sink.t) (oc : Out_channel.t)
      : unit Or_error.t =
  let model = model_for_arch ctx in
  let prog = ctx.config.cmd in
  let argv = make_argv model path in
  Or_error.tag ~tag:"While running herd"
    (Run.Local.run ~oc ~prog argv)
  end)

let run (ctx : t)
    ~(path : Fpath.t) ~sink : unit Or_error.t =
  Filter.run
    ctx
    (Io.In_source.of_fpath path)
    sink
;;

let run_and_load_results (ctx : t)
    ~(input_path : Fpath.t) ~(output_path : Fpath.t) =
  let open Or_error.Let_syntax in
  let%bind () = run ctx ~path:input_path ~sink:(Io.Out_sink.file output_path) in
  Herd_output.load ~path:output_path
;;
