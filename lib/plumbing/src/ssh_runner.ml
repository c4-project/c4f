(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

open struct
  module Tx = Travesty_base_exts
end

module Config = struct
  type t = {remote: Ssh.t; copy_dir: string}
  [@@deriving sexp, make, fields, equal]

  let host (ssh : t) : string = Ssh.host (remote ssh)

  let user (ssh : t) : string option = Ssh.user (remote ssh)

  let pp : t Fmt.t =
    Fmt.(
      using user (option (string ++ any "@@"))
      ++ using host string ++ any ":" ++ using copy_dir string)

  let of_string (s : string) : t =
    let ssh_s, copy_dir = String.rsplit2_exn s ~on:':' in
    let remote = Ssh.of_string ssh_s in
    make ~remote ~copy_dir
end

module Make (C : sig
  val config : Config.t
end) : Runner_types.S = Runner.Make (struct
  let remote_cfg : Ssh.t = Config.remote C.config

  let copy_dir : string = Config.copy_dir C.config

  let remote_file_name (local_path : Fpath.t) : string =
    (* Assuming that scp always supports unix-style paths *)
    let local_file = Fpath.filename local_path in
    sprintf "%s/%s" copy_dir local_file

  let copy_spec_to_remote :
      Fpath.t Copy_spec.t -> Copy_projection.t Copy_spec.t =
    Copy_projection.project ~f:(fun kind path ->
        match kind with
        | `Directory ->
            copy_dir
        | `File ->
            remote_file_name path)

  let scp_receive : Copy_projection.t Copy_spec.t -> unit Or_error.t =
    function
    | Directory dir ->
        let local = Copy_projection.local dir in
        let remote = Copy_projection.remote dir in
        Scp.receive remote_cfg ~recurse:true ~remotes:[remote] ~local
    | Files [] | Nothing ->
        Ok ()
    | Files fs ->
        (* TODO(@MattWindsor91): work out how to get multi-file receive
           without having to break it into separate receives. *)
        Tx.Or_error.combine_map_unit fs ~f:(fun file ->
            let local = Copy_projection.local file in
            let remote = Copy_projection.remote file in
            Scp.receive remote_cfg ~recurse:false ~remotes:[remote] ~local)

  (* TODO(@MattWindsor91): mangle the files such that duplicate files get
     non-overlapping names, forbid duplicate files, or do a two-speed thing
     where ambiguous cases turn into iterated single-file copies. *)
  let scp_send : Copy_projection.t Copy_spec.t -> unit Or_error.t = function
    | Directory dir ->
        let local = Copy_projection.local dir in
        let remote = Copy_projection.remote dir in
        Scp.send remote_cfg ~recurse:true ~locals:[local] ~remote
    | Files [] | Nothing ->
        Ok ()
    | Files [file] ->
        (* We do this to prevent ambiguity between scp-ing a file to a
           directory and scp-ing a file to a file, but, as mentioned above,
           we might want to enable this path in a loop when the fast path is
           unavailable. *)
        let local = Copy_projection.local file in
        let remote = Copy_projection.remote file in
        Scp.send remote_cfg ~recurse:false ~locals:[local] ~remote
    | Files files ->
        (* TODO(@MattWindsor91): check that all of the files have the same
           directory? *)
        let locals = List.map ~f:Copy_projection.local files in
        Scp.send remote_cfg ~recurse:false ~locals ~remote:copy_dir

  let pre (cs_pair : Fpath.t Copy_spec.Pair.t) :
      Copy_projection.t Copy_spec.Pair.t Or_error.t =
    let input' = copy_spec_to_remote cs_pair.input in
    let output' = copy_spec_to_remote cs_pair.output in
    Or_error.Let_syntax.(
      let%bind () = Copy_spec.validate_local cs_pair.input in
      let%map () = scp_send input' in
      {Copy_spec.Pair.input= input'; output= output'})

  let post ~(output : Copy_projection.t Copy_spec.t) : unit Or_error.t =
    Or_error.Let_syntax.(
      let%bind () = scp_receive output in
      Copy_spec.validate_local (Copy_projection.all_local output))

  let run_batch ?(out : Runner_output.t option) (argvs : string list list)
      ~(prog : string) : unit Or_error.t =
    Tx.Or_error.combine_map_unit
      ~f:(fun argv -> Ssh.run ?out remote_cfg ~prog ~argv)
      argvs
end)
