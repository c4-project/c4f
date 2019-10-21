(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

module Program = struct
  type t =
    { decls: Act_c_mini.Initialiser.t Act_c_mini.Named.Alist.t
    ; stms: Metadata.t Act_c_mini.Statement.t list }
  [@@deriving sexp]

  let empty : t = {decls= []; stms= []}

  let has_statements (p : t) : bool = not (List.is_empty p.stms)

  module Path : Path_types.S_function with type target := t = struct
    type target = t

    let gen_insert_stm ({stms; _} : target) :
        Path_shapes.func Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.map
        (Path.Statement_list.gen_insert_stm stms)
        ~f:Path_shapes.in_stms

    let handle_stm (path : Path_shapes.func)
        ~(f :
              Path_shapes.stm_list
           -> Metadata.t Act_c_mini.Statement.t list
           -> Metadata.t Act_c_mini.Statement.t list
              Or_error.t) (prog : t) : t Or_error.t =
      match path with
      | In_stms rest ->
          Or_error.(
            f rest prog.stms >>| fun stms' -> {prog with stms= stms'})

    let insert_stm (path : Path_shapes.func)
        (stm : Metadata.t Act_c_mini.Statement.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Path.Statement_list.insert_stm rest stm)

    let transform_stm (path : Path_shapes.func)
        ~(f :
              Metadata.t Act_c_mini.Statement.t
           -> Metadata.t Act_c_mini.Statement.t Or_error.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(Path.Statement_list.transform_stm ~f)
  end

  let statements_of_function (func : unit Act_c_mini.Function.t) :
      Metadata.t Act_c_mini.Statement.t list =
    func |> Act_c_mini.Function.body_stms
    |> List.map ~f:(
             Act_c_mini.Statement.On_meta.map
               ~f:(fun () -> Metadata.existing))

  let of_function (func : unit Act_c_mini.Function.t) : t =
    { decls= Act_c_mini.Function.body_decls func
    ; stms= statements_of_function func }

  module R_alist = Act_c_mini.Named.Alist.As_named (Var.Record)

  (** [make_function_parameters vars] creates a uniform function parameter
      list for a C litmus test using the global variable records in [vars]. *)
  let make_function_parameters (vars : Var.Map.t) :
      Act_c_mini.Type.t Act_c_mini.Named.Alist.t =
    vars
    |> Var.Map.env_satisfying_all ~scope:Global ~predicates:[]
    |> Map.to_alist

  let to_function (prog : t) ~(vars : Var.Map.t) ~(id : int) :
      unit Act_c_mini.Function.t Act_c_mini.Named.t =
    let name = Ac.C_id.of_string (Printf.sprintf "P%d" id) in
    let body_stms =
      List.map prog.stms
        ~f:Act_c_mini.Statement.erase_meta
    in
    let parameters = make_function_parameters vars in
    let func =
      Act_c_mini.Function.make ~parameters ~body_decls:prog.decls ~body_stms
        ()
    in
    Act_c_mini.Named.make func ~name

  let list_to_litmus (progs : t list) ~(vars : Var.Map.t) :
      Act_c_mini.Litmus.Lang.Program.t list =
    (* We need to filter _before_ we map, since otherwise we'll end up
       assigning the wrong thread IDs. *)
    progs
    |> List.filter ~f:has_statements
    |> List.mapi ~f:(fun id prog -> to_function ~vars ~id prog)
end

module Test = struct
  type t = (Act_c_mini.Constant.t, Program.t) Act_litmus.Test.Raw.t
  [@@deriving sexp]

  let add_new_program : t -> t =
    Act_litmus.Test.Raw.add_thread_at_end ~thread:Program.empty

  module Path : Path_types.S_program with type target := t = struct
    type target = t

    type stm = Metadata.t Act_c_mini.Statement.t

    let gen_insert_stm (test : target) :
        Path_shapes.program Base_quickcheck.Generator.t =
      let prog_gens =
        List.mapi (Act_litmus.Test.Raw.threads test) ~f:(fun index prog ->
            Base_quickcheck.Generator.map
              (Program.Path.gen_insert_stm prog)
              ~f:(Path_shapes.in_func index))
      in
      Base_quickcheck.Generator.union prog_gens

    let handle_stm (path : Path_shapes.program)
        ~(f : Path_shapes.func -> Program.t -> Program.t Or_error.t)
        (test : target) : target Or_error.t =
      match path with
      | In_func (index, rest) ->
          Act_litmus.Test.Raw.try_map_thread ~index ~f:(f rest) test

    let insert_stm (path : Path_shapes.program) (stm : stm) :
        target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Program.Path.insert_stm rest stm)

    let transform_stm (path : Path_shapes.program)
        ~(f : stm -> stm Or_error.t) : target -> target Or_error.t =
      handle_stm path ~f:(Program.Path.transform_stm ~f)
  end

  let programs_of_litmus (test : Act_c_mini.Litmus.Test.t) : Program.t list
      =
    test |> Act_c_mini.Litmus.Test.threads
    |> List.map ~f:(Fn.compose Program.of_function Act_c_mini.Named.value)

  let of_litmus (test : Act_c_mini.Litmus.Test.t) : t =
    Act_litmus.Test.Raw.make
      ~header:(Act_c_mini.Litmus.Test.header test)
      ~threads:(programs_of_litmus test)

  let to_litmus (subject : t) ~(vars : Var.Map.t) :
      Act_c_mini.Litmus.Test.t Or_error.t =
    let header = Act_litmus.Test.Raw.header subject in
    let threads = Act_litmus.Test.Raw.threads subject in
    let threads' = Program.list_to_litmus ~vars threads in
    Act_c_mini.Litmus.Test.make ~header ~threads:threads'

  let add_var_to_init (subject : t) (var : Ac.C_id.t)
      (initial_value : Act_c_mini.Constant.t) : t Or_error.t =
    Act_litmus.Test.Raw.try_map_header subject
      ~f:(Act_litmus.Header.add_global ~name:var ~initial_value)
end
