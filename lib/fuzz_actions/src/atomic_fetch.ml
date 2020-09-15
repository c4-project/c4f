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
            Fuzz.Payload_impl.Insertion.t

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

    (** A functor that produces a quickcheck instance for atomic fetchs given
        source and destination variable environments. *)
    module Quickcheck (Src : Fir.Env_types.S) (Dst : Fir.Env_types.S) :
      Act_utils.My_quickcheck.S_with_sexp
        with type t := Fir.Expression.t Fir.Atomic_fetch.t
  end) : S = Storelike.Make (struct
    let name = prefix_name Common.Id.("insert" @: B.name_suffix)

    let readme_preamble : string list = [readme_preamble; B.readme_insert]

    include B

    type t = Fir.Expression.t Fir.Atomic_fetch.t [@@deriving sexp]

    let gen ~(src : Fir.Env.t) ~(dst : Fir.Env.t) ~(vars : Fuzz.Var.Map.t)
        ~(tid : int) : t Base_quickcheck.Generator.t =
      let module Src = struct
        let env = src
      end in
      let module Dst = struct
        let env = dst
      end in
      ignore vars ;
      ignore tid ;
      let module Gen = B.Quickcheck (Src) (Dst) in
      [%quickcheck.generator: Gen.t]

    let to_stms (x : Fir.Expression.t Fir.Atomic_fetch.t) :
        Fir.Prim_statement.t list =
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
  end)

  (* TODO(@MattWindsor91): 'Int' module, which would need some care in order
     to be overflow-safe. *)

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

    module Quickcheck = Fir_gen.Expr.Atomic_fetch_int_values
  end)

  module Int_redundant : S = Make (struct
    let name_suffix = Common.Id.("int" @: "redundant" @: empty)

    let readme_insert : string =
      {| This variant can insert anywhere, but only fetches the known value of
       a destination back to itself. |}

    let path_filter = Fuzz.Path_filter.empty

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
