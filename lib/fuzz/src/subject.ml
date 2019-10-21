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

(* TODO(@MattWindsor91): deprecate this in favour of metadata *)
module With_source = struct
  type 'a t = {item: 'a; source: [`Existing | `Generated]}
  [@@deriving sexp, fields, make]
end

module Program = struct
  type t =
    { decls: Act_c_mini.Initialiser.t Act_c_mini.Named.Alist.t
    ; stms: Metadata.t Act_c_mini.Statement.t With_source.t list }
  [@@deriving sexp]

  let empty : t = {decls= []; stms= []}

  let has_statements (p : t) : bool = not (List.is_empty p.stms)

  module Stm_path :
    Path_types.S_statement
      with type target = Metadata.t Act_c_mini.Statement.t With_source.t =
  struct
    type target = Metadata.t Act_c_mini.Statement.t With_source.t

    let lower_stm = With_source.item

    let lift_stm item = With_source.make ~item ~source:`Generated

    let try_gen_insert_stm t =
      Path.Statement.try_gen_insert_stm (With_source.item t)

    let insert_stm path stm {With_source.item; source} =
      Or_error.(
        item
        |> Path.Statement.insert_stm path stm
        >>| fun item' -> With_source.make ~item:item' ~source)

    let transform_stm path ~f {With_source.item; source} =
      Or_error.(
        item
        |> Path.Statement.transform_stm path ~f
        >>| fun item' -> With_source.make ~item:item' ~source)
  end

  module Stm_list_path :
    Path_types.S_statement_list
      with type target = Metadata.t Act_c_mini.Statement.t With_source.t =
    Path.Make_statement_list (Stm_path)

  module Path : Path_types.S_function with type target := t = struct
    type target = t

    let gen_insert_stm ({stms; _} : target) :
        Path_shapes.func Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.map
        (Stm_list_path.gen_insert_stm stms)
        ~f:Path_shapes.in_stms

    let handle_stm (path : Path_shapes.func)
        ~(f :
              Path_shapes.stm_list
           -> Metadata.t Act_c_mini.Statement.t With_source.t list
           -> Metadata.t Act_c_mini.Statement.t With_source.t list
              Or_error.t) (prog : t) : t Or_error.t =
      match path with
      | In_stms rest ->
          Or_error.(
            f rest prog.stms >>| fun stms' -> {prog with stms= stms'})

    let insert_stm (path : Path_shapes.func)
        (stm : Metadata.t Act_c_mini.Statement.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(fun rest -> Stm_list_path.insert_stm rest stm)

    let transform_stm (path : Path_shapes.func)
        ~(f :
              Metadata.t Act_c_mini.Statement.t
           -> Metadata.t Act_c_mini.Statement.t Or_error.t) :
        target -> target Or_error.t =
      handle_stm path ~f:(Stm_list_path.transform_stm ~f)
  end

  let statements_of_function (func : unit Act_c_mini.Function.t) :
      Metadata.t Act_c_mini.Statement.t With_source.t list =
    func |> Act_c_mini.Function.body_stms
    |> List.map ~f:(fun item ->
           let item =
             Act_c_mini.Statement.On_meta.map
               ~f:(fun () -> Metadata.empty)
               item
           in
           With_source.make ~item ~source:`Existing)

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
        ~f:(Fn.compose Act_c_mini.Statement.erase_meta With_source.item)
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
