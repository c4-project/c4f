(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

module Make (B : Runner_types.Basic) : Runner_types.S = struct
  module Amake = Aux_maker.Make (B)

  let make_global (ctx : Context.t) (id : Ac.Litmus_id.t)
      (record : Var_map.Record.t) : Ac.C_id.t * Act_fir.Initialiser.t =
    let ty = Var_map.Record.c_type record in
    (* TODO(@MattWindsor91): is this correct? *)
    let value =
      Option.value
        (Context.lookup_initial_value ~id ctx)
        ~default:(Act_fir.Constant.zero_of_type ty)
    in
    let cid = Var_map.Record.c_id record in
    (cid, Act_fir.Initialiser.make ~ty ~value)

  let make_globals (ctx : Context.t) :
      (Act_common.C_id.t, Act_fir.Initialiser.t) List.Assoc.t =
    let vm = Context.var_map ctx in
    let global_recs = Var_map.globally_mapped_vars vm in
    List.map ~f:(fun (i, v) -> make_global ctx i v) global_recs

  let make_program (input : Act_fir.Litmus.Test.t) (context : Context.t) :
      unit Act_fir.Program.t Or_error.t =
    let raw_functions = Act_fir.Litmus.Test.threads input in
    let globals = make_globals context in
    Or_error.Let_syntax.(
      let%map function_list =
        B.Function.rewrite_all raw_functions ~context
      in
      let functions = Ac.C_named.alist_of_list function_list in
      Act_fir.Program.make ~globals ~functions)

  let make_local_init (fn : unit Act_fir.Function.t) :
      (Act_common.C_id.t, Act_fir.Constant.t) List.Assoc.t =
    fn |> Act_fir.Function.body_decls
    |> Accessor_base.(
         map (List.each @> Tuple2.snd) ~f:(get Act_fir.Initialiser.value))

  let make_local_inits :
         unit Act_fir.Function.t list
      -> ( int
         , (Act_common.C_id.t, Act_fir.Constant.t) List.Assoc.t )
         List.Assoc.t =
    List.mapi ~f:(fun tid fn -> (tid, make_local_init fn))

  let make_context (input : Act_fir.Litmus.Test.t) (aux : Aux.t) : Context.t
      =
    (* We can get the context just from looking at functions, because of the
       way in which C litmus tests are constructed. *)
    let functions =
      Accessor_base.(to_list (List.each @> Ac.C_named.value))
        (Act_fir.Litmus.Test.threads input)
    in
    let local_inits = make_local_inits functions in
    Context.make ~aux ~local_inits

  let run (input : Act_fir.Litmus.Test.t) : Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind aux = Amake.make_aux input in
      let context = make_context input aux in
      let%map program = make_program input context in
      Output.make ~program ~aux)
end
