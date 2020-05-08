(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Cm = Act_c_mini
end

module type S =
  Action_types.S with type Payload.t = Cm.Atomic_store.t Payload.Insertion.t

let readme_preamble : string =
  {|
    Generates a store operation on a randomly selected fuzzer-generated
    global variable.
  |}

(** Functor for generating variants of the store action. *)
module Make (B : sig
  val name_suffix : Ac.Id.t
  (** [name_suffix] is the name of the action, less 'store.make'. *)

  val readme_insert : string
  (** [readme_insert] is the part of the action readme specific to this form
      of the store action. *)

  val dst_type : Cm.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : (Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  (** A functor that produces a quickcheck instance for atomic stores given
      source and destination variable environments. *)
  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t := Cm.Atomic_store.t
end) :
  Action_types.S with type Payload.t = Cm.Atomic_store.t Payload.Insertion.t =
Storelike.Make (struct
  let name = Act_common.Id.("store" @: "make" @: B.name_suffix)

  let readme_preamble : string list = [readme_preamble; B.readme_insert]

  include B

  type t = Cm.Atomic_store.t [@@deriving sexp]

  let gen ~(src : Cm.Env.t) ~(dst : Cm.Env.t) ~(vars : Var.Map.t)
      ~(tid : int) : t Base_quickcheck.Generator.t =
    let module Src = struct
      let env = src
    end in
    let module Dst = struct
      let env = dst
    end in
    let module G = B.Quickcheck (Src) (Dst) in
    ignore vars ; ignore tid ; G.quickcheck_generator

  let new_locals (_ : Cm.Atomic_store.t) :
      Cm.Initialiser.t Ac.C_named.Alist.t =
    []

  let to_stms (x : Cm.Atomic_store.t) : Cm.Prim_statement.t list =
    [Cm.Prim_statement.atomic_store x]

  let dst_ids (x : Cm.Atomic_store.t) : Ac.C_id.t list =
    [Cm.Address.variable_of (Cm.Atomic_store.dst x)]

  let src_exprs (x : Cm.Atomic_store.t) : Cm.Expression.t list =
    [Cm.Atomic_store.src x]
end)

module Int : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.normal"

  let readme_insert : string =
    "This variant can insert anywhere and target any source and destination."

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Storelike.Dst_restriction.forbid_dependencies]

  module Flags = struct
    let erase_known_values = true

    let respect_src_dependencies = true
  end

  let dst_type = Cm.Type.Basic.int ~is_atomic:true ()

  module Quickcheck = Cm.Atomic_store.Quickcheck_ints
end)

module Int_dead : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.dead"

  let readme_insert : string =
    {| This variant can target any source and destination, but only inserts
       into dead code.  As it only targets dead code, it does not add
       dependences or erase known-values. |}

  let path_filter = Path_filter.(empty |> in_dead_code_only)

  let extra_dst_restrictions = []

  module Flags = struct
    let erase_known_values = false

    let respect_src_dependencies = false
  end

  let dst_type = Cm.Type.Basic.int ~is_atomic:true ()

  module Quickcheck = Cm.Atomic_store.Quickcheck_ints
end)

module Int_redundant : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.redundant"

  let readme_insert : string =
    {| This variant can insert anywhere, but only stores the known value of
       a destination back to itself. |}

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Var.Record.has_known_value]

  module Flags = struct
    let erase_known_values = false

    let respect_src_dependencies = true
  end

  let dst_type = Cm.Type.Basic.int ~is_atomic:true ()

  (* The quickcheck scheme for redundant stores needs to be very different
     from the usual scheme, as it must make sure the source is the
     destination's known value. *)
  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t = Cm.Atomic_store.t =
  struct
    type t = Cm.Atomic_store.t [@@deriving sexp]

    module Q_dst = Cm.Address_gen.Atomic_int_pointers (Dst)

    let quickcheck_observer = Cm.Atomic_store.quickcheck_observer

    (* TODO(@MattWindsor91): allow shrinking the MO? *)
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

    let known_value_expr_of_dest (dst : Q_dst.t) : Cm.Expression.t Or_error.t
        =
      Or_error.Let_syntax.(
        let id = Cm.Address.variable_of dst in
        let%bind kvo = Cm.Env.known_value Dst.env ~id in
        let%map kv =
          Result.of_option kvo
            ~error:(Error.of_string "No known value for this record.")
        in
        Cm.Expression.constant kv)

    let quickcheck_generator =
      (* Deliberately ignore the source environment. TODO(@MattWindsor91):
         optimise this? *)
      Base_quickcheck.Generator.map2 [%quickcheck.generator: Q_dst.t]
        Cm.Mem_order.gen_store ~f:(fun dst mo ->
          (* We're really hoping that the availability check over known
             values works here, because there's no way we can safely return
             an error if not. *)
          let src = Or_error.ok_exn (known_value_expr_of_dest dst) in
          Cm.Atomic_store.make ~src ~dst ~mo)
  end
end)

module Xchgify : Action_types.S with type Payload.t = Path.Program.t = struct
  let name = Act_common.Id.("store" @: "xchgify" @: empty)

  let readme () =
    {| Promotes a random atomic store to an atomic exchange whose value is
       discarded. |}

  let path_filter : Path_filter.t Lazy.t =
    lazy
      Path_filter.(
        empty
        |> require_end_check
             ~check:
               (Is_of_class
                  (Cm.Statement_class.atomic ~specifically:Store ())))

  module Payload = struct
    type t = Path.Program.t [@@deriving sexp]

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      ignore param_map ;
      let filter = Lazy.force path_filter in
      Payload.Helpers.lift_quickcheck_opt
        (Path_producers.Test.try_gen_transform_stm test ~filter)
        ~random ~action_id:name
  end

  let available (subject : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore param_map ;
    State.Monad.return
      (Path_filter.is_constructible (Lazy.force path_filter) ~subject)

  module Atoms =
    Travesty.Traversable.Chain0
      (Subject.Statement.On_primitives)
      (Cm.Prim_statement.On_atomics)
  module AtomsM = Atoms.On_monad (Or_error)

  let not_a_store_action (type a) (_ : a) : Cm.Atomic_statement.t Or_error.t
      =
    Or_error.error_string "not a store action"

  let a_store_action (s : Cm.Atomic_store.t) :
      Cm.Atomic_statement.t Or_error.t =
    Ok
      (Cm.Atomic_statement.xchg
         Cm.Atomic_store.(
           Cm.Atomic_xchg.make ~obj:(dst s) ~desired:(src s) ~mo:(mo s)))

  let xchgify_atomic :
      Cm.Atomic_statement.t -> Cm.Atomic_statement.t Or_error.t =
    Cm.Atomic_statement.reduce ~cmpxchg:not_a_store_action
      ~fence:not_a_store_action ~fetch:not_a_store_action
      ~xchg:not_a_store_action ~store:a_store_action

  let xchgify :
      Metadata.t Cm.Statement.t -> Metadata.t Cm.Statement.t Or_error.t =
    AtomsM.map_m ~f:xchgify_atomic

  let run (subject : Subject.Test.t) ~(payload : Path.Program.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.Test.transform_stm payload ~f:xchgify ~target:subject)
end
