(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lang

module type Intf = sig
  type statement

  val sanitise : statement list -> statement list list
end

module T (LS : Language.S)
       : (Intf with type statement = LS.statement) =
  struct
    type statement = LS.statement

    let remove_nops =
      List.filter ~f:(fun x -> not (LS.is_nop x))

    let remove_directives  =
      List.filter ~f:(fun x -> not (LS.is_directive x))

    let split_programs stms =
      (* Adding a nop to the start forces there to be some
         instructions before the first program, meaning we can
         simplify discarding such instructions. *)
      let progs =
        (LS.nop() :: stms)
        |> List.group ~break:(fun _ -> LS.is_program_boundary)
      in
      List.drop progs 1
    (* TODO(MattWindsor91): divine the end of the program. *)

    let make_programs_uniform nop ps =
      let maxlen =
        ps
        |> (List.max_elt ~compare:(fun x y -> Int.compare (List.length x) (List.length y)))
        |> Option.value_map ~f:(List.length) ~default:0
      in
      List.map ~f:(fun p -> p @ List.init (maxlen - List.length p)
                                          ~f:(fun _ -> nop))
               ps

    (** [sanitise_single_stm] performs sanitisation on a single program. *)
    let sanitise_program prog =
      prog
      |> remove_nops
      |> remove_directives

    let sanitise stms =
      stms
      |> split_programs
      |> List.map ~f:sanitise_program
      |> make_programs_uniform (LS.nop ())
  end
