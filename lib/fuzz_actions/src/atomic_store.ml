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
  Common.Id.("atomic" @: "store" @: rest)

module Insert = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fir.Atomic_store.t Fuzz.Payload_impl.Pathed.t

  let readme_preamble : string =
    {|
    Generates a store operation on a randomly selected fuzzer-generated
    global atomic variable.
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
      Act_utils.My_quickcheck.S_with_sexp with type t := Fir.Atomic_store.t
  end) : S = Storelike.Make (struct
    let name = prefix_name Common.Id.("insert" @: B.name_suffix)

    let readme_preamble : string list = [readme_preamble; B.readme_insert]

    include B

    (* No atomic stores are transaction-safe, so we forbid their generation
       in atomic blocks. *)
    let path_filter = Fuzz.Path_filter.not_in_atomic_block @@ path_filter

    type t = Fir.Atomic_store.t [@@deriving sexp]

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

    let new_locals (_ : Fir.Atomic_store.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      []

    let to_stms (x : Fir.Atomic_store.t) : Fir.Prim_statement.t list =
      [ Accessor.construct
          Fir.(Prim_statement.atomic @> Atomic_statement.store)
          x ]

    let dst_ids (x : Fir.Atomic_store.t) : Common.C_id.t list =
      x.@*(Fir.Atomic_store.dst @> Fir.Address.variable_of)

    let src_exprs (x : Fir.Atomic_store.t) : Fir.Expression.t list =
      x.@*(Fir.Atomic_store.src)
  end)

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

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    module Quickcheck = Fir_gen.Atomic_store.Int
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

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    module Quickcheck = Fir_gen.Atomic_store.Int
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

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    (* The quickcheck scheme for redundant stores needs to be very different
       from the usual scheme, as it must make sure the source is the
       destination's known value. *)
    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp with type t = Fir.Atomic_store.t =
    struct
      type t = Fir.Atomic_store.t [@@deriving sexp]

      module Q_dst = Fir_gen.Address.Atomic_int_pointers (Dst)

      let quickcheck_observer = Fir.Atomic_store.quickcheck_observer

      (* TODO(@MattWindsor91): allow shrinking the MO? *)
      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

      let known_value_expr_of_dest (dst : Q_dst.t) :
          Fir.Expression.t Or_error.t =
        Or_error.Let_syntax.(
          let id = dst.@(Fir.Address.variable_of) in
          let%bind kvo = Fir.Env.known_value Dst.env ~id in
          let%map kv =
            Result.of_option kvo
              ~error:(Error.of_string "No known value for this record.")
          in
          Fir.Expression.constant kv)

      let quickcheck_generator =
        (* Deliberately ignore the source environment. TODO(@MattWindsor91):
           optimise this? *)
        Base_quickcheck.Generator.map2 [%quickcheck.generator: Q_dst.t]
          Fir.Mem_order.gen_store ~f:(fun dst mo ->
            (* We're really hoping that the availability check over known
               values works here, because there's no way we can safely return
               an error if not. *)
            let src = Or_error.ok_exn (known_value_expr_of_dest dst) in
            Fir.Atomic_store.make ~src ~dst ~mo)
    end
  end)
end

module Transform = struct
  module Xchgify :
    Fuzz.Action_types.S with type Payload.t = Fuzz.Path.Flagged.t = struct
    let name = prefix_name Common.Id.("transform" @: "xchgify" @: empty)

    let readme () =
      {| Promotes a random atomic store to an atomic exchange whose value is
       discarded. |}

    let path_filter : Fuzz.Path_filter.t Lazy.t =
      lazy
        Fuzz.Path_filter.(
          empty
          |> require_end_check
               ~check:
                 (Stm_class
                    (Is, [Fir.Statement_class.atomic ~specifically:Store ()])))

    module Payload = struct
      type t = Fuzz.Path.Flagged.t [@@deriving sexp]

      let gen : t Fuzz.Payload_gen.t =
        let filter = Lazy.force path_filter in
        Fuzz.Payload_gen.path_with_flags Transform ~filter
    end

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible (Lazy.force path_filter)
        ~kind:Transform

    module Atoms =
      Travesty.Traversable.Chain0
        (Fuzz.Subject.Statement.On_primitives)
        (Fir.Prim_statement.On_atomics)
    module AtomsM = Atoms.On_monad (Or_error)

    let not_a_store_action (type a) (_ : a) :
        Fir.Atomic_statement.t Or_error.t =
      Or_error.error_string "not a store action"

    let a_store_action (s : Fir.Atomic_store.t) :
        Fir.Atomic_statement.t Or_error.t =
      Ok
        (Accessor.construct Fir.Atomic_statement.xchg
           Fir.Atomic_store.(
             Fir.Atomic_xchg.make ~obj:s.@(dst) ~desired:s.@(src) ~mo:s.@(mo)))

    let xchgify_atomic :
        Fir.Atomic_statement.t -> Fir.Atomic_statement.t Or_error.t =
      Fir.Atomic_statement.value_map ~cmpxchg:not_a_store_action
        ~fence:not_a_store_action ~fetch:not_a_store_action
        ~xchg:not_a_store_action ~store:a_store_action

    let xchgify :
           Fuzz.Metadata.t Fir.Statement.t
        -> Fuzz.Metadata.t Fir.Statement.t Or_error.t =
      AtomsM.map_m ~f:xchgify_atomic

    let run (subject : Fuzz.Subject.Test.t) ~(payload : Fuzz.Path.Flagged.t)
        : Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.Monadic.return
        (Fuzz.Path_consumers.consume_with_flags subject
           ~filter:(Lazy.force path_filter) ~path:payload
           ~action:(Transform xchgify))
  end
end
