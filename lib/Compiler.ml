open Core
open Rresult

type compilation =
  { cc_id    : string
  ; cc_spec  : CompilerSpec.t
  ; in_path  : string
  ; out_path : string
  }

let run_cc (cc : string) (argv : string list) =
  let proc = Unix.create_process ~prog:cc
                                 ~args:argv
  in
  let errch = Unix.in_channel_of_descr proc.stderr in
  let res =
    Unix.waitpid proc.pid
    |> R.reword_error
         (function
          | `Exit_non_zero ret ->
             let outp = Stdio.In_channel.input_lines errch in
             R.msgf "'%s' exited with code %d with error:\n%s"
                    (String.concat ~sep:" " (cc::argv))
                    ret
                    (String.concat ~sep:"\n" outp)
          | `Signal sg ->
             R.msgf "'%s' caught signal %s"
                    (String.concat ~sep:" " (cc::argv))
                    (Signal.to_string sg))
  in
  Stdio.In_channel.close errch;
  res

let compile_gcc (cmp : compilation) =
  let final_argv =
  [ "-S"       (* emit assembly *)
  ; "-fno-pic" (* don't emit position-independent code *)
  ]
  @ cmp.cc_spec.argv
  @ [ "-o"
    ; cmp.out_path
    ; cmp.in_path
    ] in
  run_cc cmp.cc_spec.cmd final_argv

let compile (cc_id : string) (cc_spec : CompilerSpec.t) (ps : Pathset.t) =
  let asm_path = List.Assoc.find_exn ps.a_paths cc_id ~equal:(=) in
  let cmp = { cc_id = cc_id
            ; cc_spec = cc_spec
            ; in_path = ps.c_path
            ; out_path = asm_path
            } in
  match cc_spec.style with
  | Gcc -> compile_gcc cmp

let test (cc_spec : CompilerSpec.t) =
  match cc_spec.style with
  | Gcc -> run_cc cc_spec.cmd ["--version"]
