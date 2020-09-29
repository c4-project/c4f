(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t = (module Action_types.S)

module With_default_weight = struct
  type t = {action: (module Action_types.S); default_weight: int}
  [@@deriving make, fields]

  let ( @-> ) (action : (module Action_types.S)) (default_weight : int) =
    {action; default_weight}

  let name ({action= (module M); _} : t) : Act_common.Id.t = M.name
end

module Adjusted_weight = struct
  type t = Not_adjusted of int | Adjusted of {original: int; actual: int}

  let make ?(user_weight : int option) ~(default_weight : int) : t =
    Option.value_map user_weight
      ~f:(fun actual -> Adjusted {original= default_weight; actual})
      ~default:(Not_adjusted default_weight)

  let pp_weight (f : Formatter.t) : int -> unit =
    Fmt.(
      function
      | 0 ->
          styled (`Fg `Red) (any "disabled") f ()
      | k ->
          styled (`Fg `Green) (int ++ any "x") f k)

  let pp (f : Formatter.t) : t -> unit = function
    | Not_adjusted o ->
        pp_weight f o
    | Adjusted {original; actual} ->
        Fmt.pf f "%a (normally %a)" pp_weight actual pp_weight original
end

module Summary = struct
  type t = {weight: Adjusted_weight.t; readme: string}
  [@@deriving fields, make]

  let of_action ?(user_weight : int option)
      ({action= (module M); default_weight} : With_default_weight.t) : t =
    let weight = Adjusted_weight.make ~default_weight ?user_weight in
    let readme = M.readme () in
    {weight; readme}

  let pp (f : Formatter.t) ({weight; readme} : t) : unit =
    Fmt.pf f "@[<v>@[Weight:@ %a@]@,@[<hv 2>Summary:@ @[%a@]@]@]"
      Adjusted_weight.pp weight Fmt.paragraphs readme

  let pp_map : t Map.M(Act_common.Id).t Fmt.t = Act_common.Id.pp_map pp

  let pp_map_terse : t Map.M(Act_common.Id).t Fmt.t =
    Fmt.(
      using Map.to_alist
        (list ~sep:sp
           (hbox
              (pair ~sep:(any ":@ ") Act_common.Id.pp
                 (using (fun x -> x.weight) Adjusted_weight.pp)))))
end

module Pool = struct
  type t = With_default_weight.t Utils.Weighted_list.t

  let of_weighted_actions
      (weighted_actions : (With_default_weight.t, int option) List.Assoc.t) :
      t Or_error.t =
    (* TODO(@MattWindsor91): ideally we shouldn't lose whether the weight was
       overridden or not, even if the final weight equals the default one. *)
    weighted_actions
    |> List.map ~f:(fun (act, override) ->
           ( act
           , Option.value override
               ~default:(With_default_weight.default_weight act) ))
    |> Utils.Weighted_list.from_alist

  let summarise : t -> Summary.t Map.M(Common.Id).t =
    Utils.Weighted_list.fold
      ~init:(Map.empty (module Common.Id))
      ~f:(fun map (action : With_default_weight.t) weight ->
        (* TODO(@MattWindsor91): see TODO above, as it pertains to this bit
           too. *)
        let user_weight =
          if weight = With_default_weight.default_weight action then None
          else Some weight
        in
        Map.set map
          ~key:(With_default_weight.name action)
          ~data:(Summary.of_action action ?user_weight))

  let remove (table : t) (module C : Action_types.S) : t Or_error.t =
    (* TODO(@MattWindsor91): this is quite inefficient. *)
    Utils.Weighted_list.adjust_weights table
      ~f:(fun {action= (module C'); _} w ->
        if Common.Id.equal C.name C'.name then 0 else w)

  let pick (table : t) ~(random : Splittable_random.State.t) :
      (t * (module Action_types.S)) Or_error.t =
    Or_error.Let_syntax.(
      let%bind {action; _} = Utils.Weighted_list.sample table ~random in
      let%map table' = remove table action in
      (table', action))
end

module Make_surround (Basic : sig
  val name : Act_common.Id.t

  val surround_with : string

  val readme_suffix : string

  module Payload : sig
    type t [@@deriving sexp]

    val gen : Path.Flagged.t -> t Payload_gen.t

    val src_exprs : t -> Act_fir.Expression.t list
  end

  val available : Availability.t

  val path_filter : State.t -> Path_filter.t

  val checkable_path_filter : Path_filter.t

  val run_pre :
    Subject.Test.t -> payload:Payload.t -> Subject.Test.t State.Monad.t

  val wrap :
    Subject.Statement.t list -> payload:Payload.t -> Subject.Statement.t
end) :
  Action_types.S with type Payload.t = Basic.Payload.t Payload_impl.Pathed.t =
struct
  let name = Basic.name

  let available = Basic.available

  let readme () =
    Act_utils.My_string.format_for_readme
      (String.concat
         [ {| This action removes a sublist of statements from the program, replacing
          them with |}
         ; Basic.surround_with
         ; {| containing those statements. |}
         ; "\n\n"
         ; Basic.readme_suffix ])

  module Payload = struct
    type t = Basic.Payload.t Payload_impl.Pathed.t [@@deriving sexp]

    let gen =
      Payload_impl.Pathed.gen Transform_list Basic.path_filter
        Basic.Payload.gen
  end

  let add_dependencies ({where; payload} : Payload.t) : unit State.Monad.t =
    let src_exprs = Basic.Payload.src_exprs payload in
    State.Monad.add_expression_dependencies_at_path src_exprs ~path:where

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () = add_dependencies payload in
        let%bind test' = Basic.run_pre test ~payload:payload.payload in
        (* TODO(@MattWindsor91): work out how to get the full path filter
           over here *)
        Payload_impl.Pathed.surround ~filter:Basic.checkable_path_filter
          payload ~test:test' ~f:(fun payload -> Basic.wrap ~payload)))
end

module Make_log (B : sig
  val name : Act_common.Id.t
end) : sig
  val log : Act_common.Output.t -> ('a, Formatter.t, unit) format -> 'a
end = struct
  let log (o : Act_common.Output.t) (format : ('a, Formatter.t, unit) format)
      : 'a =
    Fmt.(
      Act_common.Output.pv o
        Caml.("@[<h>%a:@ @[" ^^ format ^^ "@]@]@.")
        (styled (`Fg `Green) Act_common.Id.pp)
        B.name)
end
