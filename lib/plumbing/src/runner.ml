(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Quick and easy process running *)

open Core
open Runner_types

let argv_one_file (fn : (string, 'a) argv_fun) :
    (string Copy_spec.t, 'a) argv_fun =
 fun ~input ~output ->
  match (input, output) with
  | Files [infile], Files [outfile] ->
      fn ~input:infile ~output:outfile
  | _, _ ->
      Or_error.error_string "Expected one input file and one output file"

module Make (B : Basic) : S = struct
  include B

  let run ?oc argv ~prog = run_batch ?oc ~prog [argv]

  let run_batch_with_copy ?(oc : Out_channel.t option)
      (cs_pair : Fpath.t Copy_spec.Pair.t)
      (argvs_f : (string Copy_spec.t, string list list) argv_fun)
      ~(prog : string) =
    let open Or_error.Let_syntax in
    let%bind {input; output} = B.pre cs_pair in
    let%bind argvs = argvs_f ~input ~output in
    let%bind () = run_batch ?oc ~prog argvs in
    B.post cs_pair.output

  let run_with_copy ?(oc : Out_channel.t option)
      (cs_pair : Fpath.t Copy_spec.Pair.t)
      (argv_f : (string Copy_spec.t, string list) argv_fun) ~(prog : string)
      =
    let open Or_error.Let_syntax in
    let%bind {input; output} = B.pre cs_pair in
    let%bind argv = argv_f ~input ~output in
    let%bind () = run ?oc ~prog argv in
    B.post cs_pair.output
end

module Local : S = Make (struct
  let post = Copy_spec.validate_local

  let pre (cs_pair : Fpath.t Copy_spec.Pair.t) :
      string Copy_spec.Pair.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map () = Copy_spec.validate_local cs_pair.input in
    Copy_spec.Pair.map ~f:Fpath.to_string cs_pair

  let process_status (prog : string) (args : string list) :
      Low_level_process.Status.t -> unit Or_error.t = function
    | `Timeout _ ->
        Or_error.error_string "timed out"
    | `Exited 0 ->
        Result.ok_unit
    | `Exited code ->
        Or_error.error_s
          [%message
            "process exited with nonzero status" ~prog
              ~args:(args : string list)
              ~code:(code : int)]
    | `Signaled signal ->
        Or_error.error_s
          [%message
            "process caught signal" ~prog
              ~args:(args : string list)
              ~signal:(signal : Signal.t)]

  let run_single stdoutf stderrf prog args =
    let open Or_error.Let_syntax in
    let%bind out =
      Or_error.tag_arg
        (Or_error.try_with_join (fun () ->
             return (Low_level_process.run ~prog ~args ~stdoutf ~stderrf ())))
        "Failed to run a child process:" (prog :: args)
        [%sexp_of: string list]
    in
    process_status prog args out.status

  let run_batch ?oc (argss : string list list) ~prog =
    let stdoutf =
      match oc with
      | None ->
          Fn.const (Fn.const ())
      | Some o ->
          fun buf len -> Out_channel.output o ~buf ~pos:0 ~len
    in
    let stderrf buf len =
      Out_channel.output Out_channel.stderr ~buf ~pos:0 ~len
    in
    let results = List.map argss ~f:(run_single stdoutf stderrf prog) in
    Or_error.combine_errors_unit results
end)
