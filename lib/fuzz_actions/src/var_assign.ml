(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("var" @: "assign" @: rest)

module Insert = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fir.Assign.t Fuzz.Payload_impl.Insertion.t

  let readme_preamble : string =
    {|
    Generates an assign operation on a randomly selected fuzzer-generated
    variable.
  |}

  (** Functor for generating variants of the store action. *)
  module Make (B : sig
    val name_suffix : Common.Id.t
    (** [name_suffix] is the name of the action, less 'store.make'. *)

    val readme_insert : string
    (** [readme_insert] is the part of the action readme specific to this
        form of the store action. *)

    val dst_type : Fir.Type.Basic.t
    (** [dst_type] is the value type of the destination. *)

    val path_filter : Fuzz.Path_filter.t
    (** [path_filter] is the filter to apply on statement insertion paths
        before considering them for the atomic store. *)

    val extra_dst_restrictions : (Fuzz.Var.Record.t -> bool) list
    (** [extra_dst_restrictions] is a list of additional restrictions to
        place on the destination variables (for example, 'must not have
        dependencies'). *)

    module Flags : Storelike_types.Flags

    (** A functor that produces a quickcheck instance for atomic stores given
        source and destination variable environments. *)
    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp with type t := Fir.Assign.t
  end) : S = Storelike.Make (struct
    let name = prefix_name Common.Id.("insert" @: B.name_suffix)

    let readme_preamble : string list = [readme_preamble; B.readme_insert]

    include B

    (* TODO(@MattWindsor91): assigns that don't involve pointers are
       transaction-safe, probably. *)
    let path_filter = Fuzz.Path_filter.not_in_atomic_block @@ path_filter

    type t = Fir.Assign.t [@@deriving sexp]

    let gen ~(src : Fir.Env.t) ~(dst : Fir.Env.t) ~(vars : Fuzz.Var.Map.t)
        ~(tid : int) : t Base_quickcheck.Generator.t =
      let module Src = struct
        let env = src
      end in
      let module Dst = struct
        let env = dst
      end in
      let module G = B.Quickcheck (Src) (Dst) in
      ignore vars ; ignore tid ; G.quickcheck_generator

    let new_locals (_ : Fir.Assign.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      []

    let to_stms (x : Fir.Assign.t) : Fir.Prim_statement.t list =
      [Accessor.construct Fir.Prim_statement.assign x]

    let dst_ids (x : Fir.Assign.t) : Common.C_id.t list =
      x.@*(Fir.Assign.dst @> Fir.Lvalue.variable_of)

    let src_exprs (x : Fir.Assign.t) : Fir.Expression.t list =
      x.@*(Fir.Assign.src @> Fir.Assign.Source.exprs)
  end)

  (* TODO(@MattWindsor91): de-duplicate with Atomic_store *)

  module Int_normal : S = Make (struct
    let name_suffix = Common.Id.("int" @: "normal" @: empty)

    let readme_insert : string =
      "This variant can insert anywhere and target any source and \
       destination."

    let path_filter = Fuzz.Path_filter.empty

    let extra_dst_restrictions =
      [Storelike.Dst_restriction.forbid_dependencies]

    module Flags = struct
      let erase_known_values = true

      let execute_multi_safe = `If_no_cycles
    end

    let dst_type = Fir.Type.Basic.int ~is_atomic:false ()

    module Quickcheck = Fir_gen.Assign.Int
  end)

  module Int_dead : S = Make (struct
    let name_suffix = Common.Id.("int" @: "dead" @: empty)

    let readme_insert : string =
      {| This variant can target any source and destination, but only inserts
       into dead code.  As it only targets dead code, it does not add
       dependences or erase known-values. |}

    let path_filter = Fuzz.Path_filter.(empty |> in_dead_code_only)

    let extra_dst_restrictions = []

    module Flags = struct
      let erase_known_values = false

      let execute_multi_safe = `Always
    end

    let dst_type = Fir.Type.Basic.int ~is_atomic:false ()

    module Quickcheck = Fir_gen.Assign.Int
  end)

  module Int_redundant : S = Make (struct
    let name_suffix = Common.Id.("int" @: "redundant" @: empty)

    let readme_insert : string =
      {| This variant can insert anywhere, but only stores the known value of
       a destination back to itself. |}

    let path_filter = Fuzz.Path_filter.empty

    let extra_dst_restrictions = [Fuzz.Var.Record.has_known_value]

    module Flags = struct
      let erase_known_values = false

      let execute_multi_safe = `Always
    end

    let dst_type = Fir.Type.Basic.int ~is_atomic:false ()

    (* The quickcheck scheme for redundant stores needs to be very different
       from the usual scheme, as it must make sure the source is the
       destination's known value. *)
    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp with type t = Fir.Assign.t = struct
      type t = Fir.Assign.t [@@deriving sexp]

      module Q_dst = Fir_gen.Lvalue.Int_values (Dst)

      let quickcheck_observer = Fir.Assign.quickcheck_observer

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

      let known_value_expr_of_dest (dst : Q_dst.t) :
          Fir.Expression.t Or_error.t =
        Or_error.Let_syntax.(
          let id = dst.@(Fir.Lvalue.variable_of) in
          let%bind kvo = Fir.Env.known_value Dst.env ~id in
          let%map kv =
            Result.of_option kvo
              ~error:(Error.of_string "No known value for this record.")
          in
          Fir.Expression.constant kv)

      let quickcheck_generator =
        (* Deliberately ignore the source environment. TODO(@MattWindsor91):
           optimise this? *)
        Base_quickcheck.Generator.map [%quickcheck.generator: Q_dst.t]
          ~f:(fun dst ->
            (* We're really hoping that the availability check over known
               values works here, because there's no way we can safely return
               an error if not. *)
            let src =
              Fir.Assign.Source.Expr
                (Or_error.ok_exn (known_value_expr_of_dest dst))
            in
            Fir.Assign.make ~src ~dst)
    end
  end)
end
