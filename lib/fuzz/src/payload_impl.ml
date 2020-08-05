(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Insertion = struct
  type 'a t = {to_insert: 'a; where: Path.Test.t}
  [@@deriving make, fields, sexp]

  module Make (Basic : sig
    type t [@@deriving sexp]

    val path_filter : Availability.Context.t -> Path_filter.t

    val gen : Path.Test.t -> t Payload_gen.t
  end) =
  struct
    open struct
      type 'a ins = 'a t [@@deriving sexp]
    end

    type t = Basic.t ins [@@deriving sexp]

    let gen : t Payload_gen.t =
      (* We can't do this inside the Opt-gen applicative, as we need the
         ability to pull out the path and use it in the main generator. *)
      Payload_gen.(
        let* filter =
          lift
            (Fn.compose Basic.path_filter
               Payload_gen.Context.to_availability)
        in
        let* where = path Insert ~filter in
        let+ to_insert = Basic.gen where in
        make ~to_insert ~where)
  end
end

module Cond_surround = struct
  module Body = struct
    type t = {cond: Act_fir.Expression.t; where: Path.Test.t}
    [@@deriving make, sexp, fields]
  end

  include Body

  (** [add_cond_dependencies path cond] marks every variable in [cond] as
      having a read dependency, using [path] to resolve the scope at which
      [cond] is being inserted.

      This is a considerable over-approximation of the actual needed
      dependencies. *)
  let add_cond_dependencies (path : Path.Test.t)
      (cond : Act_fir.Expression.t) : unit State.Monad.t =
    (* TODO(@MattWindsor91): it would be pretty cool for the lvalues to track
       whether or not they are being used in a tautology, once we add
       metadata tracking to the expression. In this case, we could skip
       adding dependencies where not necessary. *)
    State.Monad.add_expression_dependencies cond
      ~scope:(Local (Path.Test.tid path))

  let apply ?(filter : Path_filter.t option) ({cond; where} : t)
      ~(test : Subject.Test.t)
      ~(f :
            Act_fir.Expression.t
         -> Subject.Statement.t list
         -> Subject.Statement.t) : Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () = add_cond_dependencies where cond in
        Monadic.return
          (Path_consumers.consume test ?filter ~path:where
             ~action:(Transform_list (fun test -> Ok [f cond test])))))

  let cond_env (vars : Var.Map.t) ~(tid : int) : Act_fir.Env.t =
    Var.Map.env_satisfying_all ~scope:(Local tid) ~predicates:[] vars

  module Make (Basic : sig
    val cond_gen :
      Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given a first-class environment module [env]
        capturing the variables in scope at the point where the if statement
        is appearing, return a Quickcheck generator generating expressions
        over those variables. *)

    val path_filter : Availability.Context.t -> Path_filter.t
  end) : Payload_types.S with type t = Body.t = struct
    include Body

    let gen_cond (path : Path.Test.t) : Act_fir.Expression.t Payload_gen.t =
      let tid = Path.Test.tid path in
      Payload_gen.(
        let* vars = lift (Fn.compose State.vars Context.state) in
        let env = cond_env vars ~tid in
        lift_quickcheck (Basic.cond_gen env))

    let gen : Body.t Payload_gen.t =
      Payload_gen.(
        let* filter =
          lift (Fn.compose Basic.path_filter Context.to_availability)
        in
        let* where = Payload_gen.path Transform_list ~filter in
        let+ cond = gen_cond where in
        Body.make ~cond ~where)
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
