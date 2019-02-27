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

module With_source = struct
  type 'a t = { item : 'a; source : [ `Existing | `Generated ] }
      [@@deriving sexp, fields, make]
end

module Program = struct
  type t =
    { decls : Mini.Initialiser.t Mini.id_assoc
    ; stms  : (Mini.Statement.t With_source.t) list
    }
  [@@deriving sexp]
  ;;

  let empty () : t = { decls = []; stms = [] }

  module Stm_path : Mini_path.S_statement
    with type target = Mini.Statement.t With_source.t = struct
    type target = Mini.Statement.t With_source.t

    let lower_stm = With_source.item
    let lift_stm item = With_source.make ~item ~source:`Generated

    let try_gen_insert_stm t =
      Mini_path.Statement.try_gen_insert_stm (With_source.item t)
    ;;

    let insert_stm path stm { With_source.item; source } =
      Or_error.(
        item |> Mini_path.Statement.insert_stm path stm >>|
        (fun item' -> With_source.make ~item:item' ~source)
      )
    ;;

    let transform_stm path ~f { With_source.item; source } =
      Or_error.(
        item |> Mini_path.Statement.transform_stm path ~f >>|
        (fun item' -> With_source.make ~item:item' ~source)
      )
    ;;
  end

  module Stm_list_path : Mini_path.S_statement_list
    with type target = Mini.Statement.t With_source.t =
    Mini_path.Make_statement_list (Stm_path)
  ;;

  module Path : Mini_path.S_function with type target := t = struct
    type target = t

    let gen_insert_stm ({ stms; _ } : target)
      : Mini_path.stm_hole Mini_path.function_path Quickcheck.Generator.t =
      Quickcheck.Generator.map (Stm_list_path.gen_insert_stm stms)
        ~f:(fun path -> Mini_path.On_statements path)
    ;;

    let handle_stm (type a)
        (path : a Mini_path.function_path)
        ~(f
          :  a Mini_path.list_path
          -> Mini.Statement.t With_source.t list
          -> Mini.Statement.t With_source.t list Or_error.t
         )
        (prog : t)
      : t Or_error.t =
      match path with
      | On_statements rest ->
        Or_error.(
          f rest prog.stms
          >>| (fun stms' -> { prog with stms = stms' })
        )
    ;;

    let insert_stm
        (path : Mini_path.stm_hole Mini_path.function_path)
        (stm : Mini.Statement.t)
      : target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Stm_list_path.insert_stm rest stm)
    ;;

    let transform_stm
        (path : Mini_path.on_stm Mini_path.function_path)
        ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t)
      : target -> target Or_error.t =
      handle_stm path ~f:(Stm_list_path.transform_stm ~f)
    ;;
  end

  let of_function (func : Mini.Function.t) : t =
    { decls = Mini.Function.body_decls func
    ; stms  = List.map (Mini.Function.body_stms func)
          ~f:(fun item -> With_source.make ~item ~source:`Existing)
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
    let body_stms = List.map prog.stms ~f:With_source.item in
    let func =
      Mini.Function.make
        ~parameters
        ~body_decls:prog.decls
        ~body_stms
        ()
    in (name, func)
  ;;
end

module Test = struct
  type t =
    { init     : Mini.Constant.t Mini.id_assoc
    ; programs : Program.t list
    } [@@deriving sexp]
  ;;

  let add_new_program (test : t) : t =
    { test with programs = (Program.empty ()) :: test.programs }
  ;;

  module Path : Mini_path.S_program with type target := t = struct
    type target = t
    type stm = Mini.Statement.t

    let gen_insert_stm (test : target)
      : Mini_path.stm_hole Mini_path.program_path Quickcheck.Generator.t =
      let prog_gens =
        List.mapi test.programs
          ~f:(fun index prog ->
              Quickcheck.Generator.map
                (Program.Path.gen_insert_stm prog)
                ~f:(fun rest -> Mini_path.On_program { index; rest })
            )
      in Quickcheck.Generator.union prog_gens
    ;;

    let handle_stm (type a)
        (path : a Mini_path.program_path)
        ~(f : a Mini_path.function_path -> Program.t -> Program.t Or_error.t)
        (test : target) : target Or_error.t =
      let open Or_error.Let_syntax in
      match path with
      | On_program { index; rest } ->
        let programs = test.programs in
        let%map programs' = Alter_list.replace programs index
            ~f:(fun func -> func |> f rest >>| Option.some)
        in { test with programs = programs' }
    ;;

    let insert_stm
        (path : Mini_path.stm_hole Mini_path.program_path)
        (stm : stm) : target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Program.Path.insert_stm rest stm)
    ;;

    let transform_stm
        (path : Mini_path.on_stm Mini_path.program_path)
        ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t)
        : target -> target Or_error.t =
      handle_stm path ~f:(Program.Path.transform_stm ~f)
    ;;
  end

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
