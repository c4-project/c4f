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
  Common.Id.("atomic" @: "fetch" @: rest)

module Insert = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t =
            Act_fir.Expression.t Act_fir.Atomic_fetch.t
            Fuzz.Payload_impl.Pathed.t

  let readme_preamble : string =
    {|
    Generates a fetch operation on a randomly selected fuzzer-generated
    global variable.
  |}

  (** Functor for generating variants of the fetch action. *)
  module Make (B : sig
    val name_suffix : Common.Id.t
    (** [name_suffix] is the name of the action, less 'fetch.make'. *)

    val readme_insert : string
    (** [readme_insert] is the part of the action readme specific to this
        form of the fetch action. *)

    val dst_type : Fir.Type.Basic.t
    (** [dst_type] is the value type of the destination. *)

    val path_filter : Fuzz.Path_filter.t
    (** [path_filter] is the filter to apply on statement insertion paths
        before considering them for the atomic fetch. *)

    val extra_dst_restrictions : (Fuzz.Var.Record.t -> bool) list
    (** [extra_dst_restrictions] is a list of additional restrictions to
        place on the destination variables (for example, 'must not have
        dependencies'). *)

    module Flags : Storelike_types.Flags

    (** A functor that produces a quickcheck instance for atomic fetches given
        source and destination variable environments. *)
    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp
        with type t := Fir.Expression.t Fir.Atomic_fetch.t
  end) : S = Storelike.Make (struct
    let name = prefix_name Common.Id.("insert" @: B.name_suffix)

    let readme_preamble : string list = [readme_preamble; B.readme_insert]

    include B

    type t = Fir.Expression.t Fir.Atomic_fetch.t [@@deriving sexp]

    let gen ~(src : Fir.Env.t) ~(dst : Fir.Env.t) ~vars:(_ : Fuzz.Var.Map.t)
        ~tid:(_ : int) : t Base_quickcheck.Generator.t =
      let module Src = struct
        let env = src
      end in
      let module Dst = struct
        let env = dst
      end in
      let module Gen = B.Quickcheck (Src) (Dst) in
      [%quickcheck.generator: Gen.t]

    let to_stms (x : Fir.Expression.t Fir.Atomic_fetch.t) :
      meta:Fuzz.Metadata.t -> Fuzz.Subject.Statement.t list =
      Storelike.lift_prims
      [ Accessor.construct
          Fir.(Prim_statement.atomic @> Atomic_statement.fetch)
          x ]

    let new_locals (_ : Fir.Expression.t Fir.Atomic_fetch.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      []

    let dst_ids (x : Fir.Expression.t Fir.Atomic_fetch.t) :
        Common.C_id.t list =
      x.@*(Fir.Atomic_fetch.obj @> Fir.Address.variable_of)

    let src_exprs (x : Fir.Expression.t Fir.Atomic_fetch.t) :
        Fir.Expression.t list =
      x.@*(Fir.Atomic_fetch.arg)

    let recommendations
        (_ : Fir.Expression.t Fir.Atomic_fetch.t Fuzz.Payload_impl.Pathed.t)
        : Common.Id.t list =
      []
  end)

  (* TODO(@MattWindsor91): 'Int' module, which would need some care in order
     to be overflow-safe. *)

  module Int_dead : S = Make (struct
    let name_suffix = Common.Id.("int" @: "dead" @: empty)

    let readme_insert : string =
      {| This variant can target any source and destination, but only inserts
       into dead code.  As it only targets dead code, it does not add
       dependences or erase known-values. |}

    let path_filter = Fuzz.Path_filter.require_flag In_dead_code

    let extra_dst_restrictions = []

    module Flags = struct
      let erase_known_values = false

      let execute_multi_safe = `Always
    end

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    module Quickcheck = Fir_gen.Expr.Atomic_fetch_int_values
  end)

  module Int_redundant : S = Make (struct
    let name_suffix = Common.Id.("int" @: "redundant" @: empty)

    let readme_insert : string =
      {| This variant can insert anywhere, but only fetches the known value of
       a destination back to itself. |}

    let path_filter = Fuzz.Path_filter.zero

    let extra_dst_restrictions = [Fuzz.Var.Record.has_known_value]

    module Flags = struct
      let erase_known_values = false

      let execute_multi_safe = `Always
    end

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp
        with type t = Fir.Expression.t Fir.Atomic_fetch.t =
      Fir_gen.Expr.Atomic_fetch_int_nops (Dst) (Src)
  end)
end

module Cond_insert = struct
  module Payload = struct
    type t =
      { fetch: Fir.Expression.t Fir.Atomic_fetch.t
      ; comparator: Fir.Op.Binary.Rel.t
      ; target: Fir.Expression.t
      } [@@deriving sexp]
  end

  module type S = 
    Fuzz.Action_types.S
      with type Payload.t =
            Payload.t
            Fuzz.Payload_impl.Pathed.t

  module Make_cond (Basic : sig
    val name_suffix : Common.Id.t
    (** [name_suffix] is the suffix of the action name. *)

    val readme_preamble : string list
    (** [readme_preamble] is the part of the action readme specific to this
        form of the fetch-conditional action. *)

    val gen_inner : Fir.Constant.t -> (Fir.Op.Fetch.t * Fir.Constant.t * Fir.Op.Binary.Rel.t * Fir.Constant.t) Q.Generator.t
    (** [gen_inner kv] should, given the known value [kv], produce a fetch operator,
        fetch argument, comparator operator, and target. *)
  end) : S = Storelike.Make (struct
    include Payload
    let name : Common.Id.t = Common.Id.(prefix_name ("insert" @: "cond" @: Basic.name_suffix))

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) : Common.Id.t list =
      []

    let readme_preamble = Basic.readme_preamble

    let to_cond ({ fetch; comparator; target} : t) : Fir.Expression.t =
      Fir.Expression.bop
        (Rel comparator)
        (Fir.Expression.atomic_fetch fetch)
        target

    let new_locals (_ : t) : Fir.Initialiser.t Common.C_named.Alist.t =
      []

    let dst_ids ({ fetch; _} : t) : Common.C_id.t list =
      [ fetch.@(Fir.Atomic_fetch.obj @> Fir.Address.lvalue_of @> Fir.Lvalue.variable_of)]

    let src_exprs ({ fetch; target; _} : t) : Fir.Expression.t list =
      (* TODO(@MattWindsor91): is this correct? *)
      [ fetch.@(Fir.Atomic_fetch.arg)
      ; target
      ]

    let to_stms (p : t) ~(meta: Fuzz.Metadata.t) : Fuzz.Subject.Statement.t list =
      [ Accessor.construct Fir.Statement.if_stm
          (Fir.If.make
             ~cond:(to_cond p)
             ~t_branch:(Fir.Block.make ~metadata:meta ())
             ~f_branch:(Fir.Block.make ~metadata:meta ())
          )
      ]

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.zero

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    let extra_dst_restrictions = [ Fuzz.Var.Record.has_known_value; Fuzz.Var.Record.can_safely_modify ]

    module Flags = struct
      let erase_known_values = false

      (* This is to prevent underflows/overflows, as we specifically calculate
         the possible addends against a known value. *)
      let execute_multi_safe = `Never
    end

    let gen ~src:(_: Fir.Env.t) ~(dst: Fir.Env.t) ~vars:(_ : Fuzz.Var.Map.t) ~tid:(_ : int) : t Q.Generator.t =
      (* TODO(@MattWindsor91): unify with the modular atomic-fetch generators? *)
      Q.Generator.Let_syntax.(
      (* This should generate atomic_ints by construction of [dst]. *)
      let%bind rec_id = Fir.Env.gen_random_var_with_record dst in
      let typed_id = Common.C_named.map_right ~f:(Accessor.get Fir.Env.Record.type_of) rec_id in
      let obj = Fir.Address.on_address_of_typed_id typed_id in
      let kv = rec_id.@?(Common.C_named.value @> Fir.Env.Record.known_value) in
      (* The variable should have a known value, but the type system doesn't
         know this. *)
      let%bind mo = Fir.Mem_order.quickcheck_generator in
      let%map (op, arg_k, comparator, target_k) = Basic.gen_inner (Option.value kv ~default:(Fir.Constant.zero_of_type typed_id.value)) in
      let arg = Fir.Expression.constant arg_k in
      let target = Fir.Expression.constant target_k in
      { fetch= Fir.Atomic_fetch.make ~obj ~arg ~mo ~op
      ; comparator
      ; target
      })
  end)
end
