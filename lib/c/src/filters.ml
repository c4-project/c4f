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

  (** Delitmusified AST *)
  type del

  module Frontend : Au.Loadable_intf.S with type t = ast

  val normal_tmp_file_ext : string

  val process : ast -> t Or_error.t

  val fuzz :
       ?seed:int
    -> t
    -> o:Ac.Output.t
    -> config:Act_config.Fuzz.t
    -> t Or_error.t

  val cvars : t -> Ac.C_variables.Map.t
  (** [cvars vast] should return a list of C identifiers corresponding to
      all variables in [vast].

      [cvars] _may_ de-litmusify the AST in the process; if you already have
      a delitmusified AST, use [cvars_of_delitmus]. *)

  val cvars_of_delitmus : del -> Ac.C_variables.Map.t
  (** [cvars_of_delitmus dl] should return a list of C identifiers
      corresponding to all variables in [dl]. *)

  val postcondition : t -> Mini_litmus.Ast.Postcondition.t option
  (** [postcondition vast] should get the Litmus postcondition of [vast], if
      one exists. *)

  val print : Out_channel.t -> t -> unit

  val delitmus : t -> del Or_error.t

  val pp_del : del Fmt.t
end

type mode =
  | Print of [`All | `Vars]
  | Delitmus
  | Fuzz of {seed: int option; o: Ac.Output.t; config: Act_config.Fuzz.t}

module Output = struct
  type t =
    { cvars: Ac.C_variables.Map.t
    ; post: Mini_litmus.Ast.Postcondition.t option }
  [@@deriving fields]
end

module type S =
  Plumbing.Filter_types.S with type aux_i = mode and type aux_o = Output.t

module Make (B : Basic) : S = Pb.Filter.Make (struct
  type aux_i = mode

  type aux_o = Output.t

  let name = "C transformer"

  let tmp_file_ext (ctx : mode Pb.Filter_context.t) : string =
    match Pb.Filter_context.aux ctx with
    | Print `All ->
        B.normal_tmp_file_ext
    | Print `Vars ->
        "txt"
    | Delitmus ->
        "c"
    | Fuzz _ ->
        "c.litmus"

  let run_delitmus (vast : B.t) (oc : Out_channel.t) :
      Ac.C_variables.Map.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map dl = B.delitmus vast in
    Fmt.pf (Format.formatter_of_out_channel oc) "%a@." B.pp_del dl ;
    B.cvars_of_delitmus dl

  let run_fuzz ?(seed : int option) (vast : B.t) (oc : Out_channel.t)
      ~(o : Ac.Output.t) ~(config : Act_config.Fuzz.t) :
      Ac.C_variables.Map.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map fz = B.fuzz ?seed ~o ~config vast in
    B.print oc fz ; B.cvars vast

  let print_all (oc : Out_channel.t) _ (x : B.t) : unit = B.print oc x

  let print_vars (oc : Out_channel.t) (vars : Ac.C_id.t list) _ : unit =
    let f = Caml.Format.formatter_of_out_channel oc in
    Fmt.(vbox (list ~sep:sp Ac.C_id.pp)) f vars

  let print :
      [`All | `Vars] -> Out_channel.t -> Ac.C_id.t list -> B.t -> unit =
    function
    | `All ->
        print_all
    | `Vars ->
        print_vars

  let run_print (output_mode : [`All | `Vars]) (vast : B.t)
      (oc : Out_channel.t) : Ac.C_variables.Map.t Or_error.t =
    let cvars = B.cvars vast in
    print output_mode oc (Ac.C_id.Map.keys cvars) vast ;
    Or_error.return cvars

  let run (ctx : mode Pb.Filter_context.t) ic oc : Output.t Or_error.t =
    let aux = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    Or_error.Let_syntax.(
      let%bind ast =
        B.Frontend.load_from_ic ~path:(Pb.Input.to_string input) ic
      in
      let%bind vast = B.process ast in
      let f =
        match aux with
        | Print output_mode ->
            run_print output_mode
        | Delitmus ->
            run_delitmus
        | Fuzz {seed; o; config} ->
            run_fuzz ?seed ~o ~config
      in
      let%map cvars = f vast oc in
      {Output.cvars; post= B.postcondition vast})
end)

module Normal_C : S = Make (struct
  type ast = Ast.Translation_unit.t

  type t = Mini.Program.t

  type del = Nothing.t (* Can't delitmus a C file *)

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

  let cvars prog =
    let raw_cvars = Mini.Program.cvars prog in
    Ac.C_variables.Map.of_single_scope_set raw_cvars

  let cvars_of_delitmus = Nothing.unreachable_code

  let postcondition = Fn.const None

  let delitmus (_ : t) : del Or_error.t =
    Or_error.error_string "Can't delitmus a normal C file"

  let pp_del _ (x : del) : unit = Nothing.unreachable_code x
end)

module Litmus : S = Make (struct
  type ast = Ast.Litmus.t

  type t = Mini_litmus.Ast.Validated.t

  type del = Delitmus.Output.t

  let normal_tmp_file_ext = "litmus"

  module Frontend = Act_c_lang.Frontend.Litmus

  let print = Mini_litmus.Pp.print

  let process = Mini_convert.litmus_of_raw_ast

  let prelude : string list =
    [ "// <!> Auto-generated from a litmus test by act."
    ; "#include <stdatomic.h>"
    ; "" ]

  let pp_prelude : unit Fmt.t =
    Fmt.(const (vbox (list ~sep:sp string)) prelude)

  let pp_del : del Fmt.t =
    Fmt.(
      prefix pp_prelude
        (using
           (Fn.compose Mini_reify.program Delitmus.Output.program)
           (vbox Ast.Translation_unit.pp)))

  let delitmus = Delitmus.run

  let fuzz :
         ?seed:int
      -> t
      -> o:Ac.Output.t
      -> config:Act_config.Fuzz.t
      -> t Or_error.t =
    Fuzzer.run

  let postcondition :
      Mini_litmus.Ast.Validated.t -> Mini_litmus.Ast.Postcondition.t option
      =
    Mini_litmus.Ast.Validated.postcondition

  let cvars_of_delitmus : del -> Ac.C_variables.Map.t =
    Delitmus.Output.c_variables

  let cvars : t -> Ac.C_variables.Map.t = Mini_litmus.cvars
end)

let c_module (is_c : bool) : (module S) =
  if is_c then (module Normal_C) else (module Litmus)
