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

open Core_kernel
open Utils

module Program = struct
  type t =
    { decls : Mini.Initialiser.t Mini.id_assoc
    ; stms  : Mini.Statement.t list
    }
  ;;

  let of_function (func : Mini.Function.t) : t =
    { decls = Mini.Function.body_decls func
    ; stms  = Mini.Function.body_stms func
    }
  ;;

  let try_extract_parameter_type
    ((n, var) : C_identifier.t * Fuzzer_var.Record.t)
    : (C_identifier.t * Mini.Type.t) Or_error.t =
    Or_error.(
      var
      |> Fuzzer_var.Record.ty
      |> Result.of_option
        ~error:(Error.of_string "Internal error: missing global type")
      >>| Tuple2.create n
    )
  ;;

  (** [make_function_parameters vars] creates a uniform function
      parameter list for a C litmus test using the global
      variable records in [vars]. *)
  let make_function_parameters
    (vars : Fuzzer_var.Map.t)
    : Mini.Type.t Mini.id_assoc Or_error.t =
    vars
    |> C_identifier.Map.filter ~f:(Fuzzer_var.Record.is_global)
    |> C_identifier.Map.to_alist
    |> List.map ~f:try_extract_parameter_type
    |> Or_error.combine_errors
  ;;

  (** [to_function prog ~vars ~id] lifts a subject-program [prog]
      with ID [prog_id]
      back into a Litmus function, adding a parameter list generated
      from [vars]. *)
  let to_function
      (prog : t)
      ~(vars : Fuzzer_var.Map.t)
      ~(id : int)
       : Mini.Function.t Mini.named Or_error.t =
    let open Or_error.Let_syntax in
    let name = C_identifier.of_string (sprintf "P%d" id) in
    let%map parameters = make_function_parameters vars in
    let func =
      Mini.Function.make
        ~parameters
        ~body_decls:prog.decls
        ~body_stms:prog.stms
        ()
    in (name, func)
  ;;
end

module Test = struct
  type t =
    { init     : Mini.Constant.t Mini.id_assoc
    ; programs : Program.t list
    }
  ;;

  let programs_of_litmus (test : Mini_litmus.Ast.Validated.t)
    : Program.t list =
    test
    |> Mini_litmus.Ast.Validated.programs
    |> List.map ~f:(fun (_, p) -> Program.of_function p)
  ;;

  (** [of_litmus test] converts a validated C litmus test [test]
      to the intermediate form used for fuzzing. *)
  let of_litmus (test : Mini_litmus.Ast.Validated.t) : t =
    { init     = Mini_litmus.Ast.Validated.init test
    ; programs = programs_of_litmus test
    }
  ;;

  let programs_to_litmus
      (progs : Program.t list)
      ~(vars : Fuzzer_var.Map.t)
    : Mini_litmus.Lang.Program.t list Or_error.t =
    progs
    |> List.mapi ~f:(fun id -> Program.to_function ~vars ~id)
    |> Or_error.combine_errors
  ;;

  (** [to_litmus ?post subject ~vars ~name] tries to reconstitute a
     validated C litmus test from the subject [subject], attaching the
     name [name] and optional postcondition [post], and using the
     variable map [vars] to reconstitute parameters. It may fail if
     the resulting litmus is invalid---generally, this signifies an
     internal error. *)
  let to_litmus
      ?(post : Mini_litmus.Ast.Post.t option)
      (subject : t)
      ~(vars : Fuzzer_var.Map.t)
      ~(name : string)
    : Mini_litmus.Ast.Validated.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind programs = programs_to_litmus ~vars subject.programs in
    Mini_litmus.Ast.Validated.make
      ?post
      ~name
      ~init:(subject.init)
      ~programs
      ()
  ;;

  (** [add_var_to_init subject var initial_value] adds [var] to
      [subject]'s init block with the initial value [initial_value]. *)
  let add_var_to_init
      (subject : t)
      (var : C_identifier.t)
      (initial_value : Mini.Constant.t)
    : t =
    { subject with init = (var, initial_value) :: subject.init }
  ;;
end
