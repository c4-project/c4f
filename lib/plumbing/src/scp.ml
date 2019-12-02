(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let scp_stanza (ssh : Ssh.t) (file : string) : string =
  (* TODO(@MattWindsor91): escaping? *)
  Printf.sprintf "%s:%s" (Ssh.to_string ssh) file

module Using_runner (R : Runner_types.S) : Scp_types.S = struct
  (* Jane Street's Shell module has scp support, but it only goes in one
     direction (send), and it only supports one send at a time. *)

  let run ~(recurse : bool) ~(sources : string list) ~(target : string) =
    let flags =
      ["-q" (* quiet mode *); "-B" (* batch mode *)]
      @ if recurse then ["-r" (* recursive *)] else []
    in
    let argv = flags @ sources @ [target] in
    R.run ~prog:"scp" argv

  let send (ssh : Ssh.t) ~(recurse : bool) ~(locals : Fpath.t list)
      ~(remote : string) =
    (* TODO(@MattWindsor91): checking remote is a directory if needed? *)
    let sources = List.map ~f:Fpath.to_string locals in
    let target = scp_stanza ssh remote in
    run ~recurse ~sources ~target

  let receive_inner (ssh : Ssh.t) ~(recurse : bool) ~(remotes : string list)
      ~(local : Fpath.t) : unit Or_error.t =
    let sources = List.map remotes ~f:(scp_stanza ssh) in
    let target = Fpath.to_string local in
    run ~recurse ~sources ~target

  let receive (ssh : Ssh.t) ~(recurse : bool) ~(remotes : string list)
      ~(local : Fpath.t) : unit Or_error.t =
    match remotes with
    | [] ->
        Ok ()
    | _ :: _ :: _ when not (Fpath.is_dir_path local) ->
        Or_error.error_string
          "Can't receive multiple files into a local file; use a directory \
           instead."
    | _ ->
        receive_inner ssh ~recurse ~remotes ~local
end

include Using_runner (Runner.Local)
