(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Quick and easy process running *)

open Core
open Core_extended

include Runner_intf

module Make (B : Basic) : S = struct
  include B

  let run ?oc ~prog argv = run_batch ?oc ~prog [argv]
end

module Local : S = Make (struct
  let process_status
      (prog : string)
      (args : string list)
    : Process.Status.t -> unit Or_error.t = function
    | `Timeout _ ->
      Or_error.error_string "timed out"
    | `Exited 0 -> Result.ok_unit
    | `Exited code ->
      Or_error.error_s
        [%message
          "process exited with nonzero status"
            ~prog
            ~args:(args : string list)
            ~code:(code : int)
        ]
    | `Signaled signal ->
      Or_error.error_s
        [%message
          "process caught signal"
            ~prog
            ~args:(args : string list)
            ~signal:(signal : Signal.t)
        ]
  ;;

  let run_single stdoutf stderrf prog args =
    let open Or_error.Let_syntax in
    let%bind out =
      Or_error.tag_arg
        (Or_error.try_with_join
           (fun () -> return (Process.run ~prog ~args ~stdoutf ~stderrf ())))
        "Failed to run a child process:"
        (prog::args)
        [%sexp_of: string list]
    in process_status prog args out.status
  ;;

  let run_batch ?oc ~prog argss =
    let stdoutf =
      match oc with
      | None -> Fn.const (Fn.const ())
      | Some o -> fun buf len -> Out_channel.output o ~buf ~pos:0 ~len
    in
    let stderrf buf len =
      Out_channel.output Out_channel.stderr ~buf ~pos:0 ~len
    in
    let results = List.map argss ~f:(run_single stdoutf stderrf prog) in
    Or_error.combine_errors_unit results
  ;;
end)
