(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

module Disable = struct
  module Location = struct
    type t = Machine | Spec

    let to_string : t -> string = function
      | Machine ->
          "machine"
      | Spec ->
          "spec"

    let pp : t Fmt.t = Fmt.of_to_string to_string
  end

  module Reason = struct
    type t = In_config | Filtered | Failed_test of Error.t

    let pp (formatter : Formatter.t) : t -> unit = function
      | In_config ->
          Fmt.pf formatter "explicitly disabled in config"
      | Filtered ->
          Fmt.pf formatter "didn't match filtering predicates"
      | Failed_test err ->
          Fmt.pf formatter "self-test failed:@ %a" Error.pp err
  end

  type t = {location: Location.t; reason: Reason.t} [@@deriving fields, make]

  let make_failed ~(location : Location.t) ~(error : Error.t) : t =
    make ~location ~reason:(Failed_test error)

  let pp : t Fmt.t =
    Fmt.(
      concat ~sep:sp
        [ any "Disabled at"
        ; styled (`Fg `Red) (using location Location.pp)
        ; any "level:"
        ; hvbox (styled (`Fg `Magenta) (using reason Reason.pp)) ])
end

type 'spec t =
  { disabled: ('spec Act_common.Spec.With_id.t * Disable.t) list
  ; enabled: 'spec Act_common.Spec.Set.t [@main] }
[@@deriving fields, make]

let get_with_fqid ?(id_type : string option)
    ?(default_machines : Act_common.Id.t list = []) (listing : 'spec t)
    ~(fqid : Act_common.Id.t) : 'spec Or_error.t =
  (* TODO(@MattWindsor91): on error, try looking up in the disabled specs,
     and emit a helpful error. *)
  Act_common.Spec.Set.get_with_fqid ?id_type ~prefixes:default_machines
    (enabled listing) ~fqid

let spec_list (set : 'spec Act_common.Spec.Set.t) : 'spec list =
  Act_common.Spec.Set.map set ~f:Act_common.Spec.With_id.spec

let enabled_spec_list (listing : 'spec t) : 'spec list =
  listing |> enabled |> spec_list

let pp_id (type spec) (sep : spec Qualified.t Fmt.t) : spec Qualified.t Fmt.t
    =
  Fmt.(
    using Qualified.m_spec_id (styled (`Fg `Red) Act_common.Id.pp)
    ++ sep
    ++ using Qualified.spec_id (styled (`Fg `Blue) Act_common.Id.pp))

let pp_qualified_summary_entry (pp_body : 'spec Fmt.t) :
    'spec Qualified.t Fmt.t =
  Fmt.(pp_id sp ++ sp ++ using Qualified.spec_without_id pp_body)

let pp_qualified_summary (pp_body : 'spec Fmt.t) : 'spec Qualified.t t Fmt.t
    =
  Fmt.(
    using enabled_spec_list
      (list ~sep:sp (hbox (pp_qualified_summary_entry pp_body))))

let pp_qualified_enabled_verbose (pp_body : 'spec Fmt.t) :
    'spec Qualified.t Act_common.Spec.Set.t Fmt.t =
  Fmt.(
    using Act_common.Spec.Set.On_specs.to_list
      (list ~sep:sp
         (vbox ~indent:1
            ( hbox (pp_id (any "/"))
            ++ cut
            ++ using Qualified.spec_without_id pp_body ))))

let pp_qualified_disabled_verbose (pp_body : 'spec Fmt.t) :
    ('spec Qualified.t Act_common.Spec.With_id.t, Disable.t) List.Assoc.t
    Fmt.t =
  Tx.Fn.Compose_syntax.(
    Fmt.(
      list ~sep:sp
        (vbox ~indent:1
           (concat ~sep:cut
              [ hbox (using (fst >> Ac.Spec.With_id.spec) (pp_id (any "/")))
              ; hbox (using snd Disable.pp)
              ; using
                  (fst >> Ac.Spec.With_id.spec >> Qualified.spec_without_id)
                  pp_body ]))))

let pp_qualified_verbose ?(type_str : string = "results")
    (pp_body : 'spec Fmt.t) : 'spec Qualified.t t Fmt.t =
  Fmt.(
    record
      [ field ("Matching " ^ type_str) enabled
          (vbox (pp_qualified_enabled_verbose pp_body))
      ; field
          ("These " ^ type_str ^ " didn't match")
          disabled
          (vbox (pp_qualified_disabled_verbose pp_body)) ])
