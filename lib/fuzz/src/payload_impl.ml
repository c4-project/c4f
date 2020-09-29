(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Pathed = struct
  type 'a t = {payload: 'a; where: Path.Flagged.t}
  [@@deriving accessors, sexp]

  (* Can't derive this, it'll derive the arguments in the wrong order. *)
  let make (payload : 'a) ~(where : Path.Flagged.t) : 'a t = {payload; where}

  let gen (kind : Path_kind.t) (path_filter : State.t -> Path_filter.t)
      (gen_inner : Path.Flagged.t -> 'a Payload_gen.t) : 'a t Payload_gen.t =
    Payload_gen.(
      let* filter = lift_state path_filter in
      let* where = path_with_flags kind ~filter in
      let+ payload = gen_inner where in
      make payload ~where)

  let surround ?(filter : Path_filter.t option) ({payload; where} : 'a t)
      ~(test : Subject.Test.t)
      ~(f : 'a -> Subject.Statement.t list -> Subject.Statement.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.consume_with_flags test ?filter ~path:where
         ~action:(Transform_list (fun test -> Ok [f payload test])))

  let insert ?(filter : Path_filter.t option) ({payload; where} : 'a t)
      ~(test : Subject.Test.t) ~(f : 'a -> Subject.Statement.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.consume_with_flags test ?filter ~path:where
         ~action:(Insert [f payload]))
end

module Cond_pathed = struct
  type t = Fir.Expression.t Pathed.t [@@deriving sexp]

  let surround ?(filter : Path_filter.t option) ({payload; where} : t)
      ~(test : Subject.Test.t)
      ~(f :
            Act_fir.Expression.t
         -> Subject.Statement.t list
         -> Subject.Statement.t) : Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () =
          State.Monad.add_expression_dependencies_at_path [payload]
            ~path:where
        in
        Pathed.surround ?filter {payload; where} ~test ~f))

  let insert ?(filter : Path_filter.t option) ({payload; where} : t)
      ~(test : Subject.Test.t)
      ~(f : Act_fir.Expression.t -> Subject.Statement.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () =
          State.Monad.add_expression_dependencies_at_path [payload]
            ~path:where
        in
        Pathed.insert ?filter {payload; where} ~test ~f))

  let cond_env (vars : Var.Map.t) ~(tid : int) : Act_fir.Env.t =
    Var.Map.env_satisfying_all ~scope:(Local tid) ~predicates:[] vars

  let lift_cond_gen
      (cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t)
      : (Path.Flagged.t -> Fir.Expression.t Payload_gen.t) Staged.t =
    Staged.stage (fun {Path_flag.Flagged.path; _} ->
        let tid = Path.tid path in
        Payload_gen.(
          let* vars = lift_acc (Context.state @> State.vars) in
          let env = cond_env vars ~tid in
          lift_quickcheck (cond_gen env)))

  let gen (kind : Path_kind.t) (path_filter : State.t -> Path_filter.t)
      (cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t)
      : t Payload_gen.t =
    Pathed.gen kind path_filter (Staged.unstage (lift_cond_gen cond_gen))

  module Make (Basic : sig
    val kind : Path_kind.t

    val cond_gen :
      Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t

    val path_filter : State.t -> Path_filter.t
  end) : Payload_types.S with type t = t = struct
    type t = Fir.Expression.t Pathed.t [@@deriving sexp]

    let gen = gen Basic.kind Basic.path_filter Basic.cond_gen
  end
end

module None : Payload_types.S with type t = unit = struct
  include Unit

  let gen : unit Payload_gen.t = Payload_gen.return ()
end

module Pure (S : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Base_quickcheck.Generator.t
end) : Payload_types.S with type t = S.t = struct
  include S

  let gen : t Payload_gen.t =
    Payload_gen.lift_quickcheck S.quickcheck_generator
end
