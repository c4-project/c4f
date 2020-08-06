(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Context = struct
  (* TODO(@MattWindsor91): this is very similar to the availability context;
     should the two share code? *)
  type t =
    { action_id: Act_common.Id.t
    ; subject: Subject.Test.t
    ; param_map: Param_map.t
    ; state: State.t
    ; random: Splittable_random.State.t }
  [@@deriving fields, make]

  let to_availability ({subject; param_map; state; _} : t) :
      Availability.Context.t =
    Availability.Context.make ~subject ~param_map ~state
end

type 'a t = (Context.t -> 'a Or_error.t) Staged.t

(* Thus named in case this gets ported to travesty *)
module Inner = struct
  let lift = Staged.stage

  let return (x : 'a Or_error.t) : 'a t = lift (Fn.const x)
end

let lift (type a) (f : Context.t -> a) : a t = Inner.lift (fun x -> Ok (f x))

let run (f : 'a t) ~(ctx : Context.t) : 'a Or_error.t = Staged.unstage f ctx

(* Reader monad *)
include (
  Monad.Make (struct
    type nonrec 'a t = 'a t

    let return (x : 'a) : 'a t = Inner.return (Ok x)

    let bind (g : 'a t) ~(f : 'a -> 'b t) : 'b t =
      Inner.lift (fun ctx ->
          Or_error.Let_syntax.(
            let%bind x = run g ~ctx in
            run (f x) ~ctx))

    let map' (g : 'a t) ~(f : 'a -> 'b) : 'b t =
      Inner.lift (fun ctx -> Or_error.(run g ~ctx >>| f))

    let map = `Custom map'
  end) :
    Monad.S with type 'a t := 'a t )

let ( let+ ) x f = map x ~f

let ( let* ) x f = bind x ~f

let lift_opt_gen (type a) (g : a Opt_gen.t) : a t =
  Inner.lift (fun ctx ->
      let g' =
        Or_error.tag_s g
          ~tag:
            [%message
              "Payload generator instantiation failed."
                ~action_id:(Context.action_id ctx : Act_common.Id.t)]
      in
      (* TODO(@MattWindsor91): size? *)
      let random = Context.random ctx in
      Or_error.(g' >>| Base_quickcheck.Generator.generate ~size:10 ~random))

let lift_quickcheck (type a) (g : a Base_quickcheck.Generator.t) : a t =
  lift_opt_gen (Ok g)

let path (kind : Path_kind.t) ~(filter : Path_filter.t) : Path.t t =
  lift Context.subject
  >>| Path_producers.try_gen ~filter ~kind
  >>= lift_opt_gen

let path_with_flags (kind : Path_kind.t) ~(filter : Path_filter.t) :
    Path.t Path_flag.Flagged.t t =
  lift Context.subject
  >>| Path_producers.try_gen_with_flags ~filter ~kind
  >>= lift_opt_gen

let flag (id : Act_common.Id.t) : bool t =
  let* param_map = lift Context.param_map in
  let* random = lift Context.random in
  let+ f = Inner.return (Param_map.get_flag param_map ~id) in
  Flag.eval f ~random

let vars : Var.Map.t t = lift (Fn.compose State.vars Context.state)
