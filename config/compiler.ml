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

include Compiler_intf

module Make_spec (R : Machine.Reference)
  : S_spec with module Mach = R = struct
  module Mach = R

  module M = struct
    type t =
      { enabled : bool [@default true] [@sexp_drop_default]
      ; style   : string
      ; emits   : Id.t
      ; cmd     : string
      ; argv    : string sexp_list
      ; herd    : bool [@default true] [@sexp_drop_default]
      ; machine : Mach.t [@default Mach.default]
      } [@@deriving sexp, fields]

    (* We use a different name for the getter than the one
       [@@deriving fields] infers. *)
    let is_enabled = enabled

    let pp f spec =
      Format.pp_open_vbox f 0;
      if not spec.enabled then Format.fprintf f "-- DISABLED --@,";
      My_format.pp_kv f "Style" String.pp spec.style;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Emits" Id.pp spec.emits;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Command"
        (Format.pp_print_list ~pp_sep:(Format.pp_print_space) String.pp)
        (spec.cmd :: spec.argv);
      Format.pp_print_cut f ();
      My_format.pp_kv f "Machine" Mach.pp spec.machine;
      Format.pp_print_cut f ();
      My_format.pp_kv f "Herd"
        (fun f x -> String.pp f (if x then "yes" else "no")) spec.herd;
      Format.pp_close_box f ()
    ;;

    let pp_summary =
      let facts spec =
        List.concat
          [ if enabled spec then [] else ["(DISABLED)"]
          ; if R.remoteness (machine spec) = `Remote then ["(REMOTE)"] else []
          ]
      in
      Fmt.(using facts (hbox (list ~sep:sp string)))
    ;;
  end

  include M

  module With_id = struct
    include Spec.With_id (M)

    let is_enabled w = M.is_enabled (spec w)
    let style      w = M.style      (spec w)
    let emits      w = M.emits      (spec w)
    let cmd        w = M.cmd        (spec w)
    let argv       w = M.argv       (spec w)
    let herd       w = M.herd       (spec w)
    let machine    w = M.machine    (spec w)
  end

  include Spec.Make (struct
      include M
      module With_id = With_id
    end)

  let create = M.Fields.create
end

module Cfg_spec : S_spec with type Mach.t = Id.t =
  Make_spec (Machine.Id)
;;

module Spec : S_spec with type Mach.t = Machine.Spec.With_id.t =
  Make_spec (Machine.Spec.With_id)
;;

module Property = struct
  type t =
    | Id      of Id.Property.t
    | Machine of Machine.Property.t
  [@@deriving sexp, variants]
  ;;

  let tree_docs : Property.Tree_doc.t =
    [ "id",
      { args = [ "PROPERTY" ]
      ; details =
          {| See 'identifier predicates'. |}
      }
    ; "machine",
      { args = [ "PROPERTY" ]
      ; details =
          {| See 'machine predicates'. |}
      }
    ]
  ;;

  let pp_tree : unit Fmt.t =
    Property.Tree_doc.pp tree_docs
      (List.map ~f:fst Variants.descriptions)
  ;;

  let%expect_test "all properties have documentation" =
    let num_passes =
      Variants.descriptions
      |> List.map ~f:fst
      |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
      |> List.count ~f:not
    in
    Fmt.pr "@[<v>%d@]@." num_passes;
    [%expect {| 0 |}]
  ;;

  let eval (cspec : Spec.With_id.t) = function
    | Id prop      -> Id.Property.eval (Spec.With_id.id cspec) prop
    | Machine prop -> Machine.Property.eval
                        (module Machine.Spec.With_id)
                        (Spec.With_id.machine cspec)
                        prop
  ;;

  let eval_b cspec expr = Blang.eval expr (eval cspec)
end

module type With_spec = sig
  val cspec : Spec.With_id.t
end

module type Basic_with_run_info = sig
  include Basic
  include With_spec
  module Runner : Runner.S
end

module Make (B : Basic_with_run_info) : S = struct
  include B

  let cmd = Spec.With_id.cmd B.cspec

  let compile ~(infile : Fpath.t) ~(outfile : Fpath.t) =
    let s = Spec.With_id.spec B.cspec in
    let argv_fun =
      Utils.Runner.argv_one_file
        (fun ~input ~output ->
           Or_error.return
             (B.compile_args
                ~args:(Spec.argv s)
                ~emits:(Spec.emits s) ~infile:input ~outfile:output
             )
        )
    in
    B.Runner.run_with_copy
      ~prog:cmd
      { input  = Copy_spec.file infile
      ; output = Copy_spec.file outfile
      }
      argv_fun
  ;;

  let test () = B.Runner.run ~prog:cmd B.test_args
end

module S_to_filter (S : S) :
  Utils.Filter.S with type aux_i = unit and type aux_o = unit =
  Utils.Filter.Make_files_only
     (struct
       type aux_i = unit
       type aux_o = unit
       let name = "C compiler"
       let tmp_file_ext = Fn.const "s"
       let run _ = S.compile
     end)
;;

module Make_filter (B : Basic_with_run_info) :
  Utils.Filter.S with type aux_i = unit and type aux_o = unit =
  S_to_filter (Make (B))
;;

let runner_from_spec (cspec : Spec.With_id.t) =
  Machine.Spec.With_id.runner (Spec.With_id.machine cspec)
;;

module Chain_input = struct
  type next_mode =
    [ `Preview
    | `No_compile
    | `Compile
    ]

    type 'a t =
    { file_type : File_type.t_or_infer
    ; next      : (next_mode -> 'a)
    }
  [@@deriving fields]

  let create = Fields.create
end

module Chain_with_compiler
    (Comp : Utils.Filter.S with type aux_i = unit and type aux_o = unit)
    (Onto : Utils.Filter.S)
  : Utils.Filter.S with type aux_i = Onto.aux_i Chain_input.t
                    and type aux_o = (unit option * Onto.aux_o) =
  Utils.Filter.Chain_conditional_first (struct
    module First  = Comp
    module Second = Onto
    type aux_i = Onto.aux_i Chain_input.t

    let lift_next (next : Chain_input.next_mode -> 'a)
      : unit Filter.chain_output -> 'a =
      function
      | `Checking_ahead -> next `Preview
      | `Skipped -> next `No_compile
      | `Ran () -> next `Compile
    ;;

    let select { Filter.aux; src; _ } =
      let file_type = Chain_input.file_type aux in
      let next      = Chain_input.next      aux in
      let f         = lift_next next in
      if File_type.is_c src file_type then `Both ((), f)
      else `One f
  end)
;;

let from_resolver_and_spec resolve cspec =
  let open Or_error.Let_syntax in
  let%map (module B : Basic) = resolve cspec in
  let (module Runner) = runner_from_spec cspec in
  (module
    (Make (struct
       let cspec = cspec
       include B
       module Runner = Runner
     end)) : S)
;;

module Make_resolver
    (B : Basic_resolver with type spec := Spec.With_id.t)
  : S_resolver with type spec = Spec.With_id.t
                and type 'a chain_input = 'a Chain_input.t = struct

  type spec = Spec.With_id.t
  type 'a chain_input = 'a Chain_input.t

  let from_spec =
    from_resolver_and_spec B.resolve
  ;;

  let filter_from_spec (cspec : Spec.With_id.t)
    : (module Utils.Filter.S with type aux_i = unit
                              and type aux_o = unit)
        Or_error.t
    =
    let open Or_error.Let_syntax in
    let%map (module S) = from_spec cspec in
    (module S_to_filter (S)
       : Utils.Filter.S with type aux_i = unit
                         and type aux_o = unit)
  ;;

  let chained_filter_from_spec
      (type i)
      (type o)
      (cspec : Spec.With_id.t)
      (module Onto : Filter.S with type aux_i = i and type aux_o = o)
    : (module
        Utils.Filter.S
        with type aux_i = i Chain_input.t
         and type aux_o = (unit option * o)
      ) Or_error.t =
    let open Or_error.Let_syntax in
    let%map (module F) = filter_from_spec cspec in
    (module Chain_with_compiler (F) (Onto)
       : Utils.Filter.S
         with type aux_i = i Chain_input.t
          and type aux_o = (unit option * o))
end

module Target = struct
  type t =
    [ `Spec of Spec.With_id.t
    | `Arch of Id.t
    ]
  ;;

  let arch : t -> Id.t = function
    | `Spec spec -> Spec.With_id.emits spec
    | `Arch arch -> arch
  ;;

  let ensure_spec : t -> Spec.With_id.t Or_error.t = function
    | `Spec spec -> Or_error.return spec
    | `Arch _ ->
      Or_error.error_string
        "Expected a compiler ID; got an architecture ID."
  ;;
end

module Fail (E : sig val error : Error.t end) : S = struct
  let test () = Result.ok_unit

  let compile ~infile ~outfile =
    ignore infile;
    ignore outfile;
    Result.Error E.error
end

module Make_target_resolver
    (B : Basic_resolver with type spec := Spec.With_id.t)
  : S_resolver with type spec = Target.t
                and type 'a chain_input = 'a Chain_input.t = struct
  type spec = Target.t
  type 'a chain_input = 'a Chain_input.t

  let from_spec = function
    | `Spec cspec -> from_resolver_and_spec B.resolve cspec
    | `Arch _ ->
      Or_error.return
      (module Fail (struct
           let error = Error.of_string
               "To run a compiler, you must supply a compiler ID."
         end)
          : S
      )
  ;;

  let filter_from_spec (tgt : Target.t) =
    let open Or_error.Let_syntax in
    let%map (module S) = from_spec tgt in
    (module S_to_filter (S)
       : Utils.Filter.S with type aux_i = unit
                         and type aux_o = unit)
  ;;

  let chained_filter_from_spec
      (type i)
      (type o)
      (tgt : Target.t)
      (module Onto : Filter.S with type aux_i = i and type aux_o = o) =
    let open Or_error.Let_syntax in
    let%map (module F) = filter_from_spec tgt in
    (module Chain_with_compiler (F) (Onto)
       : Utils.Filter.S
         with type aux_i = i Chain_input.t
          and type aux_o = (unit option * o))
end
