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

open Core_kernel
module Ac = Act_common
module Tx = Travesty_base_exts

module With_source = struct
  type 'a t = {item: 'a; source: [`Existing | `Generated]}
  [@@deriving sexp, fields, make]
end

module Program = struct
  type t =
    { decls: Act_c_mini.Initialiser.t Act_c_mini.Named.Alist.t
    ; stms: Act_c_mini.Statement.t With_source.t list }
  [@@deriving sexp]

  let empty () : t = {decls= []; stms= []}

  let has_statements (p : t) : bool = not (List.is_empty p.stms)

  let%expect_test "empty program has no statements" =
    Fmt.(pr "%a@." (using has_statements bool) (empty ())) ;
    [%expect {| false |}]

  module Stm_path :
    Act_c_mini.Path.S_statement
      with type target = Act_c_mini.Statement.t With_source.t = struct
    type target = Act_c_mini.Statement.t With_source.t

    let lower_stm = With_source.item

    let lift_stm item = With_source.make ~item ~source:`Generated

    let try_gen_insert_stm t =
      Act_c_mini.Path.Statement.try_gen_insert_stm (With_source.item t)

    let insert_stm path stm {With_source.item; source} =
      Or_error.(
        item
        |> Act_c_mini.Path.Statement.insert_stm path stm
        >>| fun item' -> With_source.make ~item:item' ~source)

    let transform_stm path ~f {With_source.item; source} =
      Or_error.(
        item
        |> Act_c_mini.Path.Statement.transform_stm path ~f
        >>| fun item' -> With_source.make ~item:item' ~source)
  end

  module Stm_list_path :
    Act_c_mini.Path.S_statement_list
      with type target = Act_c_mini.Statement.t With_source.t =
    Act_c_mini.Path.Make_statement_list (Stm_path)

  module Path : Act_c_mini.Path.S_function with type target := t = struct
    type target = t

    let gen_insert_stm ({stms; _} : target) :
        Act_c_mini.Path.stm_hole Act_c_mini.Path.function_path
        Quickcheck.Generator.t =
      Quickcheck.Generator.map (Stm_list_path.gen_insert_stm stms)
        ~f:(fun path -> Act_c_mini.Path.On_statements path)

    let handle_stm (type a) (path : a Act_c_mini.Path.function_path)
        ~(f :
              a Act_c_mini.Path.list_path
           -> Act_c_mini.Statement.t With_source.t list
           -> Act_c_mini.Statement.t With_source.t list Or_error.t)
        (prog : t) : t Or_error.t =
      match path with
      | On_statements rest ->
          Or_error.(
            f rest prog.stms >>| fun stms' -> {prog with stms= stms'})

    let insert_stm
        (path : Act_c_mini.Path.stm_hole Act_c_mini.Path.function_path)
        (stm : Act_c_mini.Statement.t) : target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Stm_list_path.insert_stm rest stm)

    let transform_stm
        (path : Act_c_mini.Path.on_stm Act_c_mini.Path.function_path)
        ~(f : Act_c_mini.Statement.t -> Act_c_mini.Statement.t Or_error.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(Stm_list_path.transform_stm ~f)
  end

  let of_function (func : Act_c_mini.Function.t) : t =
    { decls= Act_c_mini.Function.body_decls func
    ; stms=
        List.map (Act_c_mini.Function.body_stms func) ~f:(fun item ->
            With_source.make ~item ~source:`Existing) }

  let try_extract_parameter_type (var : Var.Record.t) :
      Act_c_mini.Type.t Or_error.t =
    var |> Var.Record.ty
    |> Result.of_option
         ~error:(Error.of_string "Internal error: missing global type")

  module R_alist = Act_c_mini.Named.Alist.As_named (Var.Record)

  (** [make_function_parameters vars] creates a uniform function parameter
      list for a C litmus test using the global variable records in [vars]. *)
  let make_function_parameters (vars : Var.Map.t) :
      Act_c_mini.Type.t Act_c_mini.Named.Alist.t Or_error.t =
    vars
    |> Ac.C_id.Map.filter ~f:Var.Record.is_global
    |> Ac.C_id.Map.to_alist
    |> Tx.Alist.With_errors.map_right_m ~f:try_extract_parameter_type

  let to_function (prog : t) ~(vars : Var.Map.t) ~(id : int) :
      Act_c_mini.Function.t Act_c_mini.Named.t Or_error.t =
    let name = Ac.C_id.of_string (sprintf "P%d" id) in
    let body_stms = List.map prog.stms ~f:With_source.item in
    Or_error.Let_syntax.(
      let%map parameters = make_function_parameters vars in
      let func =
        Act_c_mini.Function.make ~parameters ~body_decls:prog.decls
          ~body_stms ()
      in
      Act_c_mini.Named.make func ~name)
end

module Test = struct
  type t =
    { init: Act_c_lang.Ast_basic.Constant.t Act_c_mini.Named.Alist.t
    ; programs: Program.t list }
  [@@deriving sexp]

  let add_new_program (test : t) : t =
    {test with programs= Program.empty () :: test.programs}

  module Path : Act_c_mini.Path.S_program with type target := t = struct
    type target = t

    type stm = Act_c_mini.Statement.t

    let gen_insert_stm (test : target) :
        Act_c_mini.Path.stm_hole Act_c_mini.Path.program_path
        Quickcheck.Generator.t =
      let prog_gens =
        List.mapi test.programs ~f:(fun index prog ->
            Quickcheck.Generator.map (Program.Path.gen_insert_stm prog)
              ~f:(fun rest -> Act_c_mini.Path.On_program {index; rest}))
      in
      Quickcheck.Generator.union prog_gens

    let handle_stm (type a) (path : a Act_c_mini.Path.program_path)
        ~(f :
              a Act_c_mini.Path.function_path
           -> Program.t
           -> Program.t Or_error.t) (test : target) : target Or_error.t =
      let open Or_error.Let_syntax in
      match path with
      | On_program {index; rest} ->
          let programs = test.programs in
          let%map programs' =
            Tx.List.With_errors.replace_m programs index ~f:(fun func ->
                func |> f rest >>| Option.some)
          in
          {test with programs= programs'}

    let insert_stm
        (path : Act_c_mini.Path.stm_hole Act_c_mini.Path.program_path)
        (stm : stm) : target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Program.Path.insert_stm rest stm)

    let transform_stm
        (path : Act_c_mini.Path.on_stm Act_c_mini.Path.program_path)
        ~(f : Act_c_mini.Statement.t -> Act_c_mini.Statement.t Or_error.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(Program.Path.transform_stm ~f)
  end

  let programs_of_litmus (test : Act_c_mini.Litmus.Ast.Validated.t) :
      Program.t list =
    test |> Act_c_mini.Litmus.Ast.Validated.programs
    |> List.map ~f:(Fn.compose Program.of_function Act_c_mini.Named.value)

  let of_litmus (test : Act_c_mini.Litmus.Ast.Validated.t) : t =
    { init= Act_c_mini.Litmus.Ast.Validated.init test
    ; programs= programs_of_litmus test }

  let programs_to_litmus (progs : Program.t list) ~(vars : Var.Map.t) :
      Act_c_mini.Litmus.Lang.Program.t list Or_error.t =
    progs
    |> List.filter ~f:Program.has_statements
    |> List.mapi ~f:(fun id -> Program.to_function ~vars ~id)
    |> Or_error.combine_errors

  let%test_module "using sample environment" =
    ( module struct
      type r = Act_c_mini.Litmus.Lang.Program.t list Or_error.t
      [@@deriving sexp_of]

      let vars =
        Var.Map.make_existing_var_map
          (Lazy.force Act_c_mini.Env.test_env)
          Ac.C_id.Set.empty

      let run programs =
        let result = programs_to_litmus programs ~vars in
        Sexp.output_hum stdout [%sexp (result : r)]

      let%expect_test "programs_to_litmus: empty test" =
        run [] ; [%expect {| (Ok ()) |}]

      let%expect_test "programs_to_litmus: empty programs" =
        run (List.init 5 ~f:(fun _ -> Program.empty ())) ;
        [%expect {| (Ok ()) |}]
    end )

  let to_litmus
      ?(postcondition :
         Act_c_lang.Ast_basic.Constant.t Act_litmus.Postcondition.t option)
      (subject : t) ~(vars : Var.Map.t) ~(name : string) :
      Act_c_mini.Litmus.Ast.Validated.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind programs = programs_to_litmus ~vars subject.programs in
    Act_c_mini.Litmus.Ast.Validated.make ?postcondition ~name
      ~init:subject.init ~programs ()

  let add_var_to_init (subject : t) (var : Ac.C_id.t)
      (initial_value : Act_c_lang.Ast_basic.Constant.t) : t =
    {subject with init= (var, initial_value) :: subject.init}
end
