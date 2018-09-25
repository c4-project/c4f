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
open Utils.MyContainers

module type Intf = sig
  type statement

  val sanitise : statement list -> statement list list
end

let mangler =
  (* We could always just use something like Base36 here, but this
     seems a bit more human-readable. *)
  String.Escaping.escape_gen_exn
    ~escape_char:'Z'
    ~escapeworthy_map:[ '_', 'U'
                      ; '$', 'D'
                      ; '.', 'P'
                      ; 'Z', 'Z'
                      ]
let mangle ident =
  Staged.unstage mangler ident

let%expect_test "mangle: sample" =
  print_string (mangle "_foo$bar.BAZ");
  [%expect {| ZUfooZDbarZPBAZZ |}]

module T (LS : Language.Intf) =
  struct
    let remove_nops = MyList.exclude ~f:LS.Statement.is_nop
    let remove_directives = MyList.exclude ~f:LS.Statement.is_directive

    let split_programs stms =
      (* Adding a nop to the start forces there to be some
         instructions before the first program, meaning we can
         simplify discarding such instructions. *)
      let progs =
        (LS.Statement.nop() :: stms)
        |> List.group ~break:(Fn.const LS.Statement.is_program_boundary)
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
                                          ~f:(Fn.const nop))
               ps

    (** [mangle_identifiers] reduces identifiers into a form that herd
       can parse. *)
    let mangle_identifiers stm =
      LS.Statement.map_symbols ~f:mangle stm

    (** [sanitise_stm] performs sanitisation at the single statement
       level. *)
    let sanitise_stm stm =
      stm
      |> mangle_identifiers

    (** [sanitise_program] performs sanitisation on a single program. *)
    let sanitise_program prog =
      prog
      |> remove_nops
      |> remove_directives
      |> List.map ~f:sanitise_stm

    let sanitise_programs progs =
      progs
      |> List.map ~f:sanitise_program
      |> make_programs_uniform (LS.Statement.nop ())

    let sanitise stms = sanitise_programs (split_programs stms)
  end
