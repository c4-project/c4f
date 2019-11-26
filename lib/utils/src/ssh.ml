(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** [Ssh] contains types, modules, and functions for doing remote work over
    SSH. *)

open Core
module Pb = Plumbing

type t = {user: string option [@sexp.option]; host: string}
[@@deriving sexp, fields, make]

module Make (C : sig
  val ssh : t
end) : Ssh_types.S = struct
  let host = host C.ssh

  let user = user C.ssh
end

module Scp (Conf : Ssh_types.S) = struct
  open Conf

  let send ~(recurse : bool) ~(local : Fpath.t) ~(remote : string) =
    let local_str = Fpath.to_string local in
    (* Core_extended has scp, but it only goes in one direction. *)
    Or_error.(
      try_with (fun () -> Shell.scp ?user ~recurse ~host local_str remote))

  let scp_stanza file =
    (* TODO(@MattWindsor91): escaping? *)
    match user with
    | None ->
        sprintf "%s:%s" host file
    | Some u ->
        sprintf "%s@%s:%s" u host file

  let receive ~(recurse : bool) ~(remote : string) ~(local : Fpath.t) =
    (* We need to make our own implementation of backwards scp, which is
       flaky. *)
    let flags =
      ["-q" (* quiet mode *); "-B" (* batch mode *)]
      @ if recurse then ["-r" (* recursive *)] else []
    in
    let argv = flags @ [scp_stanza remote; Fpath.to_string local] in
    Pb.Runner.Local.run ~prog:"scp" argv
end

module Runner (Conf : Ssh_types.Basic_runner) : Pb.Runner_types.S =
Pb.Runner.Make (struct
  open Conf
  module Scp = Scp (Conf)

  let remote_file_name (local_path : Fpath.t) : string =
    (* Assuming that scp always supports unix-style paths *)
    let remote_dir = Conf.remote_dir local_path in
    let local_file = Fpath.filename local_path in
    sprintf "%s/%s" remote_dir local_file

  let map2_err (type a b c) (xs : a list) (ys : b list) ~(f : a -> b -> c)
      ~(error : Error.t) : c list Or_error.t =
    let map_result = List.map2 xs ys ~f in
    match map_result with
    | Ok xs ->
        Or_error.return xs
    | Unequal_lengths ->
        Result.Error error

  let scp_files ~(action : local:Fpath.t -> remote:string -> unit Or_error.t)
      (locals : Fpath.t list) (remotes : string list) : unit Or_error.t =
    Or_error.(
      map2_err locals remotes
        ~f:(fun local remote -> action ~local ~remote)
        ~error:(Error.of_string "Internal: file list length changed.")
      >>= combine_errors_unit)

  let copy_spec_to_remote : Fpath.t Pb.Copy_spec.t -> string Pb.Copy_spec.t =
    function
    | Directory local ->
        Directory (Conf.remote_dir local)
    | Files fs ->
        Files (List.map ~f:remote_file_name fs)
    | Nothing ->
        Nothing

  let scp_spec
      ~(dir_action : local:Fpath.t -> remote:string -> unit Or_error.t)
      ~(file_action : local:Fpath.t -> remote:string -> unit Or_error.t)
      (cs : Fpath.t Pb.Copy_spec.t) : string Pb.Copy_spec.t Or_error.t =
    let open Or_error.Let_syntax in
    let rcs = copy_spec_to_remote cs in
    let%map () =
      match (cs, copy_spec_to_remote cs) with
      | Directory local, Directory remote ->
          dir_action ~local ~remote
      | Files lfs, Files rfs ->
          scp_files ~action:file_action lfs rfs
      | Nothing, Nothing ->
          Ok ()
      | _, _ ->
          Or_error.error_string
            {| Internal error: copy_spec_to_remote produced a different
               type of copy spec than the input. |}
    in
    rcs

  let scp_send_spec :
      Fpath.t Pb.Copy_spec.t -> string Pb.Copy_spec.t Or_error.t =
    scp_spec ~dir_action:(Scp.send ~recurse:true)
      ~file_action:(Scp.send ~recurse:false)

  let scp_receive_spec :
      Fpath.t Pb.Copy_spec.t -> string Pb.Copy_spec.t Or_error.t =
    scp_spec
      ~dir_action:(fun ~local ~remote ->
        Scp.receive ~recurse:true ~remote ~local)
      ~file_action:(fun ~local ~remote ->
        Scp.receive ~recurse:false ~remote ~local)

  let pre (cs_pair : Fpath.t Pb.Copy_spec.Pair.t) :
      string Pb.Copy_spec.Pair.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind () = Pb.Copy_spec.validate_local cs_pair.input in
    let%map input' = scp_send_spec cs_pair.input in
    let output' = copy_spec_to_remote cs_pair.output in
    {Pb.Copy_spec.Pair.input= input'; output= output'}

  let post (cs : Fpath.t Pb.Copy_spec.t) : unit Or_error.t =
    let open Or_error.Let_syntax in
    let%bind _ = scp_receive_spec cs in
    Pb.Copy_spec.validate_local cs

  let run_one oc prog args =
    let open Or_error in
    Or_error.Let_syntax.(
      let%map output =
        tag_arg
          (try_with (fun () ->
               Shell.ssh_lines "%s %s" prog
                 (String.concat ~sep:" " args)
                 ~host ?user))
          "Error running remote command via ssh:"
          (host, Option.value ~default:"(default user)" user)
          [%sexp_of: string * string]
      in
      Option.iter ~f:(fun o -> Out_channel.output_lines o output) oc)

  let run_batch ?oc (argss : string list list) ~prog =
    let results = List.map ~f:(run_one oc prog) argss in
    Or_error.combine_errors_unit results
end)
