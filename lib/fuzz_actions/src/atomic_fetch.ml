(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("atomic" @: "fetch" @: rest)

let fetch_dst_ids (x : Fir.Expression.t Fir.Atomic_fetch.t) :
    Common.C_id.t list =
  x.@*(Fir.Atomic_fetch.obj @> Fir.Address.variable_of)

let fetch_src_exprs (x : Fir.Expression.t Fir.Atomic_fetch.t) :
    Fir.Expression.t list =
  (* Fetches are RMWs, so they form a circular dependency. *)
  Fir.Expression.address x.obj :: x.@*(Fir.Atomic_fetch.arg)

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

    (** A functor that produces a quickcheck instance for atomic fetches
        given source and destination variable environments. *)
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

    let new_local_cap : int = 0

    let new_locals (_ : Fir.Expression.t Fir.Atomic_fetch.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      []

    let dst_ids = fetch_dst_ids

    let src_exprs = fetch_src_exprs

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
      ; target: Fir.Expression.t }
    [@@deriving sexp]
  end

  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Payload.t Fuzz.Payload_impl.Pathed.t

  module Make (Basic : sig
    val name_suffix : Common.Id.t
    (** [name_suffix] is the suffix of the action name. *)

    val readme_insert : string
    (** [readme_insert] is the part of the action readme specific to this
        form of the fetch-conditional action. *)

    val gen_inner :
      int -> (Fir.Op.Fetch.t * int * Fir.Op.Binary.Rel.t * int) Q.Generator.t
    (** [gen_inner kv] should, given the known value [kv], produce a fetch
        operator, fetch argument, comparator operator, and target. *)
  end) : S = Storelike.Make (struct
    include Payload

    let name : Common.Id.t =
      Common.Id.(prefix_name ("insert" @: "cond" @: Basic.name_suffix))

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      []

    let readme_preamble : string list =
      [ {| This action inserts an if statement containing a destructive atomic
         fetch in its condition. |}
      ; Basic.readme_insert
      ; {| This action intends to trigger optimisations that replace the
           fetch-add or fetch-sub with a flag test on an atomic add or sub's
           final value, as happens on x86 in LLVM. |}
      ]

    let to_cond ({fetch; comparator; target} : t) : Fir.Expression.t =
      Fir.Expression.bop (Rel comparator)
        (Fir.Expression.atomic_fetch fetch)
        target

    let new_local_cap : int = 0

    let new_locals (_ : t) : Fir.Initialiser.t Common.C_named.Alist.t = []

    let dst_ids ({fetch; _} : t) : Common.C_id.t list = fetch_dst_ids fetch

    let src_exprs ({fetch; target; _} : t) : Fir.Expression.t list =
      (* TODO(@MattWindsor91): is this correct? *)
      target :: fetch_src_exprs fetch

    let to_stms (p : t) ~(meta : Fuzz.Metadata.t) :
        Fuzz.Subject.Statement.t list =
      [ Accessor.construct Fir.Statement.if_stm
          (Fir.If.make ~cond:(to_cond p)
             ~t_branch:(Fir.Block.make ~metadata:meta ())
             ~f_branch:(Fir.Block.make ~metadata:meta ())) ]

    let path_filter : Fuzz.Path_filter.t = Fuzz.Path_filter.zero

    let dst_type = Fir.Type.Basic.int ~is_atomic:true ()

    let extra_dst_restrictions =
      [Fuzz.Var.Record.has_known_value; Fuzz.Var.Record.can_safely_modify]

    module Flags = struct
      (* We're destructively modifying the objects. *)
      let erase_known_values = true

      (* This is to prevent underflows/overflows, as we specifically
         calculate the possible addends against a known value. *)
      let execute_multi_safe = `Never
    end

    let gen ~src:(_ : Fir.Env.t) ~(dst : Fir.Env.t)
        ~vars:(_ : Fuzz.Var.Map.t) ~tid:(_ : int) : t Q.Generator.t =
      (* TODO(@MattWindsor91): unify with the modular atomic-fetch
         generators? *)
      Q.Generator.Let_syntax.(
        (* This should generate atomic_ints by construction of [dst]. *)
        let%bind rec_id = Fir.Env.gen_random_var_with_record dst in
        let typed_id =
          Common.C_named.map_right
            ~f:(Accessor.get Fir.Env.Record.type_of)
            rec_id
        in
        let obj = Fir.Address.on_address_of_typed_id typed_id in
        let kv =
          rec_id.@?(Common.C_named.value @> Fir.Env.Record.known_value
                    @> Fir.Constant.Acc.int)
        in
        (* The variable should have a known value, but the type system
           doesn't know this. *)
        let%bind mo = Fir.Mem_order.quickcheck_generator in
        let%map op, arg_k, comparator, target_k =
          Basic.gen_inner (Option.value kv ~default:0)
        in
        let arg = Fir.Expression.int_lit arg_k in
        let target = Fir.Expression.int_lit target_k in
        {fetch= Fir.Atomic_fetch.make ~obj ~arg ~mo ~op; comparator; target})
  end)

  module Negated_addend : S = Make (struct
    let name_suffix = Common.Id.of_string "negated-addend"

    let readme_insert : string =
      {| The generated fetch subtracts from its target the same value that is
         compared against in the conditional. |}

    let gen_inner (kv : int) :
        (Fir.Op.Fetch.t * int * Fir.Op.Binary.Rel.t * int) Q.Generator.t =
      Q.Generator.Let_syntax.(
        (* TODO(@MattWindsor91): use a cautious add/sub generator of this ilk
           to create a generic fetch action. *)
        let%bind rel = Fir.Op.Binary.Rel.quickcheck_generator in
        let%map v =
          Q.Generator.int_inclusive 0 (Int.of_int32_trunc Int32.max_value)
        in
        (* Trying to avoid an underflow/overflow by always moving away from
           the sign of the known value. *)
        if kv >= 0 then (Fir.Op.Fetch.Sub, v, rel, v)
        else (Add, v, rel, neg v))
  end)

  module Boundary : S = Make (struct
    let name_suffix = Common.Id.of_string "boundary"

    let readme_insert : string =
      {| The generated fetch adds or subtracts 1 from its target and checks
         whether the result crossed a boundary with regards to 0. |}

    let gen_inner (kv : int) :
        (Fir.Op.Fetch.t * int * Fir.Op.Binary.Rel.t * int) Q.Generator.t =
      let not_max = kv < Int.of_int32_trunc Int32.max_value in
      let not_min = kv > Int.of_int32_trunc Int32.min_value in
      Q.Generator.of_list
        (List.filter_map
           ~f:(fun (cond, op, rel) -> Option.some_if cond (op, 1, rel, 0))
           [ (not_max, Fir.Op.Fetch.Add, Fir.Op.Binary.Rel.Lt)
           ; (not_max, Fir.Op.Fetch.Add, Fir.Op.Binary.Rel.Ge)
           ; (not_min, Fir.Op.Fetch.Sub, Fir.Op.Binary.Rel.Gt)
           ; (not_min, Fir.Op.Fetch.Sub, Fir.Op.Binary.Rel.Le) ])
  end)
end
