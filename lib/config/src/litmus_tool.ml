open Base

type t = {cmd: string [@default "litmus7"] [@drop_if_default]}
[@@deriving sexp, fields, make]

module M : Program.S with type t := t = struct
  let cmd = cmd

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp

  let enabled = Fn.const true

  let argv = Fn.const []

  let default () = make ()
end

include M
