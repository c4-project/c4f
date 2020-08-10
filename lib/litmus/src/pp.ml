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
end

let pp_location_stanza : Ac.C_id.t list Fmt.t =
  Fmt.(
    hbox (any "locations@ " ++ brackets (box (list ~sep:semi Ac.C_id.pp))))

let pp_init (ppk : 'k Fmt.t) : (Ac.C_id.t, 'k) List.Assoc.t Fmt.t =
  Act_utils.My_format.pp_c_braces
    Fmt.(
      list ~sep:sp (fun f (l, c) -> pf f "@[%a = %a;@]" Ac.C_id.pp l ppk c))

let spsp : type a. a Fmt.t = fun x -> Fmt.(sp ++ sp) x

let pp_threads (ppt : 't Fmt.t) : ('k, 't) Test.Raw.t Fmt.t =
  Fmt.(using Test.Raw.threads (list ~sep:spsp ppt))

let pp_top_line (langname : string) : _ Header.t Fmt.t =
  Fmt.(hbox (const string langname ++ sp ++ using Header.name string))

let pp_header_top (langname : string) (ppk : 'k Fmt.t) : 'k Header.t Fmt.t =
  Fmt.(
    concat ~sep:spsp [pp_top_line langname; using Header.init (pp_init ppk)])

let pp_header_bot (ppk : 'k Fmt.t) : 'k Header.t Fmt.t =
  Fmt.(
    concat ~sep:nop
      [ using Header.locations (option (spsp ++ pp_location_stanza))
      ; using Header.postcondition
          (option (spsp ++ Postcondition.pp ~pp_const:ppk)) ])

let pp (langname : string) (ppk : 'k Fmt.t) (ppt : 't Fmt.t) :
    ('k, 't) Test.Raw.t Fmt.t =
  Fmt.(
    vbox
      (concat ~sep:nop
         [ using Test.Raw.header (pp_header_top langname ppk)
         ; spsp
         ; pp_threads ppt
         ; using Test.Raw.header (pp_header_bot ppk) ]))
