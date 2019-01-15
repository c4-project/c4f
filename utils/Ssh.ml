(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Ssh] contains types, modules, and functions for doing remote work
    over SSH. *)

open Core
open Core_extended

module R = Runner

type t =
  { user     : string sexp_option
  ; host     : string
  } [@@deriving sexp, fields]
;;

(* The use of 'sexp_option' above makes deriving this impossible. *)
let create ?user ~host = Fields.create ~user ~host

module type S = sig
  val host : string
  val user : string sexp_option
end

module Make (C : sig val ssh : t end) = struct
  let host = host C.ssh
  let user = user C.ssh
end

module Runner (Conf : S) : Runner.S = Runner.Make (struct
  open Conf

  let run_one oc prog args =
    let open Or_error in
    let open Or_error.Let_syntax in
    let%map output =
      tag_arg
        (try_with
           (fun () ->
              Shell.ssh_lines "%s %s" prog (String.concat ~sep:" " args)
                ~host ?user))
        "Error running remote command via ssh:"
        (host, Option.value ~default:"(default user)" user)
        [%sexp_of: string * string]
    in
    Option.iter
      ~f:(fun o -> Out_channel.output_lines o output)
      oc
  ;;

  let run_batch ?oc ~prog argss =
    let results = List.map ~f:(run_one oc prog) argss in
    Or_error.combine_errors_unit results
  ;;
end)

module Scp (Conf : S) = struct
  open Conf

  let send ~local ~remote =
  (* Core_extended has scp, but it only goes in one direction. *)
  Or_error.(
    try_with
      (fun () ->
         Core_extended.Shell.scp ?user ~host local remote
      )
  )
  ;;

  let scp_stanza file =
    (* TODO(@MattWindsor91): escaping? *)
    match user with
    | None -> sprintf "%s:%s" host file
    | Some u -> sprintf "%s@%s:%s" u host file
  ;;

  let receive ~remote ~local =
    (* We need to make our own implementation of backwards scp,
       which is flaky. *)
    R.Local.run
      ~prog:"scp"
      [ "-q" (* quiet mode *)
      ; "-B" (* batch mode *)
      ; scp_stanza remote
      ; local
      ]
  ;;
end
