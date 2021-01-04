(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Validated *)

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

module Raw = struct
  type ('const, 'prog) t = {header: 'const Header.t; threads: 'prog list}
  [@@deriving fields, sexp]

  (* We could use [@@deriving make] here, but that would give us an optional
     threads field, which seems weird as a validated Litmus test needs at
     least one thread. *)
  let make = Fields.create

  module M (L : sig
    module Constant : T

    module Program : T
  end) =
  struct
    type nonrec t = (L.Constant.t, L.Program.t) t
  end

  let bump_tids (type const) (header : const Header.t) ~(from : int)
      ~(delta : int) : const Header.t =
    Header.map_tids header ~f:(fun tid ->
        if from <= tid then tid + delta else tid)

  let has_threads (type const prog) (test : (const, prog) t) : bool =
    not (List.is_empty (threads test))

  let num_threads (type const prog) (test : (const, prog) t) : int =
    List.length (threads test)

  let add_thread (type const prog) (test : (const, prog) t) ~(thread : prog)
      ~(index : int) : (const, prog) t Or_error.t =
    let threads = threads test in
    Or_error.Let_syntax.(
      let%map threads' = Tx.List.insert threads index thread in
      let header' = bump_tids (header test) ~from:index ~delta:1 in
      make ~threads:threads' ~header:header')

  let add_thread_at_end (type const prog) (test : (const, prog) t)
      ~(thread : prog) : (const, prog) t =
    (* We don't need to do any thread ID manipulation if we're at the end;
       small mercy given the O(n) program list construction. *)
    {test with threads= threads test @ [thread]}

  let map_name (type const prog) (test : (const, prog) t)
      ~(f : string -> string) : (const, prog) t =
    {test with header= Header.map_name ~f test.header}

  let try_map_header (type c1 c2 prog) (test : (c1, prog) t)
      ~(f : c1 Header.t -> c2 Header.t Or_error.t) : (c2, prog) t Or_error.t
      =
    let header = header test in
    (* We don't need to do any thread ID manipulation if we're just mapping. *)
    Or_error.Let_syntax.(
      let%map header' = f header in
      {test with header= header'})

  let try_map_threads (type const p1 p2) (test : (const, p1) t)
      ~(f : p1 -> p2 Or_error.t) : (const, p2) t Or_error.t =
    let threads = threads test in
    (* We don't need to do any thread ID manipulation if we're just mapping. *)
    Or_error.Let_syntax.(
      let%map threads' = Tx.Or_error.combine_map threads ~f in
      {test with threads= threads'})

  let try_map_thread (type const prog) (test : (const, prog) t)
      ~(index : int) ~(f : prog -> prog Or_error.t) :
      (const, prog) t Or_error.t =
    let threads = threads test in
    (* We don't need to do any thread ID manipulation if we're just mapping. *)
    Or_error.Let_syntax.(
      let%map threads' =
        Tx.List.With_errors.replace_m threads index ~f:(fun x ->
            x |> f >>| Option.some)
      in
      {test with threads= threads'})

  let remove_thread (type const prog) (test : (const, prog) t) ~(index : int)
      : (const, prog) t Or_error.t =
    let threads = threads test in
    Or_error.Let_syntax.(
      let%map threads' = Tx.List.replace threads index ~f:(Fn.const None) in
      let header' = bump_tids (header test) ~from:index ~delta:(-1) in
      make ~threads:threads' ~header:header')
end

let check_init_against_globals (type k t)
    (init : (Ac.C_id.t, k) List.Assoc.t) (globals : t Map.M(Ac.C_id).t) :
    unit Or_error.t =
  let init_keys = init |> List.map ~f:fst |> Set.of_list (module Ac.C_id) in
  let globals_keys = globals |> Map.keys |> Set.of_list (module Ac.C_id) in
  Or_error.(
    (* TODO(@MattWindsor91): really not sure what the correct direction is
       here, now. *)
    Tx.Or_error.unless_m (Set.equal globals_keys init_keys) ~f:(fun () ->
        error_s
          [%message
            "Program global variables aren't compatible with init."
              ~in_program:(globals_keys : Set.M(Ac.C_id).t)
              ~in_init:(init_keys : Set.M(Ac.C_id).t)]))

module Make (Lang : Test_types.Basic) :
  Test_types.S with module Lang = Lang and type raw = Raw.M(Lang).t = struct
  module Lang = Lang

  type raw = (Lang.Constant.t, Lang.Program.t) Raw.t [@@deriving sexp]

  include Validated.Make (struct
    type t = raw [@@deriving sexp]

    let here = [%here]

    (** [validate_init init] validates an incoming litmus test's init block. *)
    let validate_init (init : (Ac.C_id.t, Lang.Constant.t) List.Assoc.t) =
      let module V = Validate in
      let dup =
        List.find_a_dup ~compare:(Tx.Fn.on fst ~f:Ac.C_id.compare) init
      in
      let dup_to_err (k, v) =
        V.fail_s
          [%message
            "duplicate item in 'init'"
              ~location:(k : Ac.C_id.t)
              ~value:(v : Lang.Constant.t)]
      in
      Option.value_map ~default:V.pass ~f:dup_to_err dup

    (** [validate_threads ps] validates an incoming litmus test's threads. *)
    let validate_threads : Lang.Program.t list Validate.check =
      let module V = Validate in
      V.all
        [ V.booltest (Fn.non List.is_empty) ~if_false:"threads are empty"
          (* TODO(@MattWindsor91): duplicate name checking *) ]

    let validate_post : Lang.Constant.t Postcondition.t option Validate.check
        =
      (* TODO(@MattWindsor91): actual validation here? *)
      Fn.const Validate.pass

    let validate_locations : Ac.C_id.t list option Validate.check =
      (* TODO(@MattWindsor91): actual validation here? *)
      Fn.const Validate.pass

    let validate_name : string Validate.check =
     fun name ->
      if String.contains name ' ' then
        Validate.fail_s
          [%message "Litmus name contains invalid character" ~name]
      else Validate.pass

    (** [validate_post_or_location_exists] checks an incoming Litmus header
        to ensure that it has either a postcondition or a locations stanza. *)
    let validate_post_or_location_exists :
        Lang.Constant.t Header.t Validate.check =
      Validate.booltest
        (fun t ->
          Option.is_some (Header.locations t)
          || Option.is_some (Header.postcondition t))
        ~if_false:"Test must have a postcondition or location stanza."

    let variables_in_init (header : _ Header.t) : Set.M(Ac.C_id).t =
      header |> Header.init |> List.map ~f:fst |> Set.of_list (module Ac.C_id)

    let variables_in_locations (header : _ Header.t) : Set.M(Ac.C_id).t =
      header |> Header.locations
      |> Option.value_map
           ~f:(Set.of_list (module Ac.C_id))
           ~default:(Set.empty (module Ac.C_id))

    let validate_location_variables : Lang.Constant.t Header.t Validate.check
        =
     fun header ->
      let in_locations = variables_in_locations header in
      let in_init = variables_in_init header in
      let excess = Set.diff in_locations in_init in
      if Set.is_empty excess then Validate.pass
      else
        Validate.fail_s
          [%message
            "One or more locations aren't in the init."
              ~in_locations:(in_locations : Set.M(Ac.C_id).t)
              ~in_init:(in_init : Set.M(Ac.C_id).t)
              ~excess:(excess : Set.M(Ac.C_id).t)]

    let validate_header (t : Lang.Constant.t Header.t) : Validate.t =
      Validate.of_list
        [ validate_name (Header.name t)
        ; validate_init (Header.init t)
        ; validate_locations (Header.locations t)
        ; validate_post (Header.postcondition t)
        ; validate_post_or_location_exists t
        ; validate_location_variables t ]

    let validate_fields (t : t) : Validate.t =
      let w check = Validate.field_folder t check in
      Validate.of_list
        (Raw.Fields.fold ~init:[] ~header:(w validate_header)
           ~threads:(w validate_threads))

    let uniform_globals :
        Lang.Program.t list -> Lang.Type.t Map.M(Ac.C_id).t option Or_error.t
        = function
      | [] ->
          Or_error.error_string "empty threads"
      | xs ->
          (* Working on the assumption that global_vars either maps variables
             for all threads, or for none. *)
          xs
          |> List.map ~f:Lang.Program.global_vars
          |> Option.all
          |> Tx.Option.With_errors.map_m
               ~f:
                 (Act_utils.My_map.merge_with_overlap
                    ~compare:Lang.Type.compare)
          |> Or_error.tag ~tag:"Threads disagree on global variables sets."

    (** [validate_globals] checks an incoming Litmus test to ensure that, if
        its threads explicitly reference global variables, then they
        reference the same variables as both each other and the init block. *)
    let validate_globals : t Validate.check =
      Validate.of_error (fun (candidate : t) ->
          let threads = Raw.threads candidate in
          let header = Raw.header candidate in
          Or_error.(
            threads |> uniform_globals
            >>= Tx.Option.With_errors.iter_m
                  ~f:(check_init_against_globals (Header.init header))))

    let validate : t Validate.check =
      Validate.all [validate_fields; validate_globals]
  end)

  let threads : t -> Lang.Program.t list = Fn.compose Raw.threads raw

  let header : t -> Lang.Constant.t Header.t = Fn.compose Raw.header raw

  let name : t -> string = Fn.compose Header.name header

  let init : t -> (Ac.C_id.t, Lang.Constant.t) List.Assoc.t =
    Fn.compose Header.init header

  let locations : t -> Ac.C_id.t list option =
    Fn.compose Header.locations header

  let postcondition : t -> Lang.Constant.t Postcondition.t option =
    Fn.compose Header.postcondition header

  (* slight renaming *)
  let validate : raw -> t Or_error.t = create

  let make ~header ~threads = create (Raw.make ~header ~threads)

  let validate_language : Ac.C_id.t Validate.check =
    Validate.booltest
      (fun l -> String.Caseless.equal (Ac.C_id.to_string l) Lang.name)
      ~if_false:"incorrect language"

  let check_language (language : Ac.C_id.t) =
    Validate.valid_or_error language validate_language

  let of_ast
      ({Ast.language; name; decls} : (Lang.Constant.t, Lang.Program.t) Ast.t)
      : t Or_error.t =
    let threads = Ast.get_programs decls in
    Or_error.Let_syntax.(
      let%bind _ = check_language language in
      let%bind header = Ast.get_header name decls in
      make ~threads ~header)

  let try_map_raw (test : t) ~(f : raw -> raw Or_error.t) : t Or_error.t =
    Or_error.(test |> raw |> f >>= validate)

  let try_map_header (test : t)
      ~(f : Lang.Constant.t Header.t -> Lang.Constant.t Header.t Or_error.t)
      : t Or_error.t =
    try_map_raw ~f:(Raw.try_map_header ~f) test
end
