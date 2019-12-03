(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
module Tx = Travesty_base_exts

let id_prog (prog : string) ~(input : Copy_projection.t Copy_spec.t) :
    string Or_error.t =
  ignore input ; Or_error.return prog

let copy_prog (prog : string) ~(input : Copy_projection.t Copy_spec.t) :
    string Or_error.t =
  Or_error.(
    prog |> Fpath_helpers.of_string
    >>| fun x ->
    x |> Copy_projection.try_find input |> Option.value ~default:prog)

module Make (B : Runner_types.Basic) : Runner_types.S = struct
  include B

  let run ?oc args ~prog = run_batch ?oc ~prog [args]

  let run_batch_with_copy ?(oc : Out_channel.t option)
      ?(prog_f : Runner_types.prog_fun = id_prog)
      (cs_pair : Fpath.t Copy_spec.Pair.t)
      ~(argvs_f : string list list Runner_types.argv_fun) ~(prog : string) =
    Or_error.Let_syntax.(
      let%bind cs_pair' = pre cs_pair in
      let input = cs_pair'.input in
      let output = cs_pair'.output in
      let%bind prog' = prog_f prog ~input in
      let%bind argss = argvs_f ~input ~output in
      let%bind () = run_batch ?oc ~prog:prog' argss in
      post output)

  let run_with_copy ?(oc : Out_channel.t option)
      ?(prog_f : Runner_types.prog_fun option)
      (cs_pair : Fpath.t Copy_spec.Pair.t)
      ~(argv_f : string list Runner_types.argv_fun) ~(prog : string) =
    let argvs_f ~input ~output =
      Or_error.map ~f:List.return (argv_f ~input ~output)
    in
    run_batch_with_copy ?oc ?prog_f cs_pair ~prog ~argvs_f
end

let local_map_specs (cs_pair : Fpath.t Copy_spec.Pair.t) :
    Copy_projection.t Copy_spec.Pair.t Or_error.t =
  Or_error.return
    (Copy_spec.Pair.map_specs
       ~f:(Copy_projection.project ~f:(Fn.const Fpath.to_string))
       cs_pair)

let local_pre (cs_pair : Fpath.t Copy_spec.Pair.t) :
    Copy_projection.t Copy_spec.Pair.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind () = Copy_spec.validate_local cs_pair.input in
    local_map_specs cs_pair)

let local_post (output_spec : Copy_projection.t Copy_spec.t) :
    unit Or_error.t =
  Copy_spec.validate_local (Copy_projection.all_local output_spec)

module Local : Runner_types.S = Make (struct
  let post = local_post

  let pre :
         Fpath.t Copy_spec.Pair.t
      -> Copy_projection.t Copy_spec.Pair.t Or_error.t =
    local_pre

  let abnormal_status_error (prog : string) (args : string list) (code : int)
      (stderr_tail : string) : unit Or_error.t =
    Or_error.error_s
      [%message
        "process exited with nonzero status" ~prog
          ~args:(args : string list)
          ~code:(code : int)
          ~stderr_tail:(stderr_tail : string)]

  let signal_error (prog : string) (args : string list) (signal : Signal.t)
      (stderr_tail : string) : unit Or_error.t =
    Or_error.error_s
      [%message
        "process caught signal" ~prog
          ~args:(args : string list)
          ~signal:(signal : Signal.t)
          ~stderr_tail:(stderr_tail : string)]

  let process_status (prog : string) (args : string list)
      (res : Low_level_process.Command_result.t) : unit Or_error.t =
    match res.status with
    | `Timeout _ ->
        Or_error.error_string "timed out"
    | `Exited 0 ->
        Ok ()
    | `Exited code ->
        abnormal_status_error prog args code res.stderr_tail
    | `Signaled signal ->
        signal_error prog args signal res.stderr_tail

  let run_raw (prog : string) (args : string list)
      ~(stdoutf : Core.Bytes.t -> int -> unit)
      ~(stderrf : Core.Bytes.t -> int -> unit) :
      Low_level_process.Command_result.t Or_error.t =
    Or_error.(
      tag_s
        ~tag:
          [%message
            "Failed to run a child process" ~prog ~args:(args : string list)]
        (try_with (fun () ->
             Low_level_process.run ~prog ~args ~stdoutf ~stderrf ())))

  let run_single (prog : string) (args : string list)
      ~(stdoutf : Core.Bytes.t -> int -> unit)
      ~(stderrf : Core.Bytes.t -> int -> unit) : unit Or_error.t =
    Or_error.(
      run_raw prog args ~stdoutf ~stderrf >>= process_status prog args)

  let lift_oc_to_stdoutf :
      Stdio.Out_channel.t option -> (Core.Bytes.t -> int -> unit) Staged.t =
    function
    | None ->
        Staged.stage (Fn.const (Fn.const ()))
    | Some o ->
        Staged.stage (fun buf len -> Out_channel.output o ~buf ~pos:0 ~len)

  let run_batch ?oc (argss : string list list) ~prog =
    let stdoutf = Staged.unstage (lift_oc_to_stdoutf oc) in
    let stderrf buf len =
      Out_channel.output Out_channel.stderr ~buf ~pos:0 ~len
    in
    Tx.Or_error.combine_map_unit argss ~f:(run_single ~stdoutf ~stderrf prog)
end)

module Dry_run : Runner_types.S = Make (struct
  let post (_ : Copy_projection.t Copy_spec.t) : unit Or_error.t = Ok ()

  let pre :
         Fpath.t Copy_spec.Pair.t
      -> Copy_projection.t Copy_spec.Pair.t Or_error.t =
    local_map_specs

  let run (args : string list) ~(oc : Out_channel.t) ~(prog : string) : unit
      =
    Printf.fprintf oc "RUN %s%a\n" prog
      (fun oc -> List.iter ~f:(Printf.fprintf oc " %s"))
      args

  let run_batch ?(oc : Out_channel.t = Stdio.stdout)
      (argss : string list list) ~(prog : string) : unit Or_error.t =
    List.iter argss ~f:(run ~oc ~prog) ;
    Ok ()
end)
