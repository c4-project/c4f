(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

module Statement = struct
  type t = Metadata.t Act_c_mini.Statement.t [@@deriving sexp]

  module If = struct
    type t = Metadata.t Act_c_mini.Statement.If.t [@@deriving sexp]
  end

  module Loop = struct
    type t = Metadata.t Act_c_mini.Statement.While.t [@@deriving sexp]
  end

  let has_dead_code_blocks : t -> bool =
    Act_c_mini.Statement.has_blocks_with_metadata
      ~predicate:Metadata.is_dead_code

  include Act_c_mini.Statement.With_meta (Metadata)

  let has_atomic_statements : t -> bool =
    On_primitives.exists ~f:Act_c_mini.Prim_statement.is_atomic

  let has_labels : t -> bool =
    On_primitives.exists ~f:Act_c_mini.Prim_statement.is_label

  let has_non_label_prims : t -> bool =
    On_primitives.exists ~f:(Fn.non Act_c_mini.Prim_statement.is_label)

  let make_generated_prim : Act_c_mini.Prim_statement.t -> t =
    Act_c_mini.Statement.prim Metadata.generated
end

module Block = struct
  type t = (Metadata.t, Statement.t) Act_c_mini.Block.t

  let make_existing ?(statements : Statement.t list option) () : t =
    Act_c_mini.Block.make ?statements ~metadata:Metadata.existing ()

  let make_generated ?(statements : Statement.t list option) () : t =
    Act_c_mini.Block.make ?statements ~metadata:Metadata.generated ()

  let make_dead_code ?(statements : Statement.t list option) () : t =
    Act_c_mini.Block.make ?statements ~metadata:Metadata.dead_code ()
end

module Thread = struct
  type t =
    { decls: Act_c_mini.Initialiser.t Ac.C_named.Alist.t [@default []]
    ; stms: Statement.t list }
  [@@deriving sexp, make]

  let empty : t = {decls= []; stms= []}

  let add_decl ?(value : Act_c_mini.Constant.t option) (thread : t)
      ~(ty : Act_c_mini.Type.t) ~(name : Ac.C_id.t) : t =
    let decl = Act_c_mini.Initialiser.make ~ty ?value () in
    let decls' = (name, decl) :: thread.decls in
    {thread with decls= decls'}

  let has_statements (p : t) : bool = not (List.is_empty p.stms)

  let has_atomic_statements (p : t) : bool =
    List.exists p.stms ~f:Statement.has_atomic_statements

  let has_non_label_prims (p : t) : bool =
    List.exists p.stms ~f:Statement.has_non_label_prims

  let has_while_loops (p : t) : bool =
    List.exists p.stms ~f:Act_c_mini.Statement.has_while_loops

  let has_if_statements (p : t) : bool =
    List.exists p.stms ~f:Act_c_mini.Statement.has_if_statements

  let has_dead_code_blocks (p : t) : bool =
    List.exists p.stms ~f:Statement.has_dead_code_blocks

  let statements_of_function (func : unit Act_c_mini.Function.t) :
      Statement.t list =
    func |> Act_c_mini.Function.body_stms
    |> List.map
         ~f:
           (Act_c_mini.Statement.On_meta.map ~f:(fun () -> Metadata.existing))

  let of_function (func : unit Act_c_mini.Function.t) : t =
    make
      ~decls:(Act_c_mini.Function.body_decls func)
      ~stms:(statements_of_function func)
      ()

  module R_alist = Ac.C_named.Alist.As_named (Var.Record)

  (** [make_function_parameters vars] creates a uniform function parameter
      list for a C litmus test using the global variable records in [vars]. *)
  let make_function_parameters (vars : Var.Map.t) :
      Act_c_mini.Type.t Ac.C_named.Alist.t =
    vars
    |> Var.Map.env_satisfying_all ~scope:Global ~predicates:[]
    |> Act_c_mini.Env.typing
    |> Map.to_alist

  let to_function (prog : t) ~(vars : Var.Map.t) ~(id : int) :
      unit Act_c_mini.Function.t Ac.C_named.t =
    let name = Ac.C_id.of_string (Printf.sprintf "P%d" id) in
    let body_stms = List.map prog.stms ~f:Act_c_mini.Statement.erase_meta in
    let parameters = make_function_parameters vars in
    let func =
      Act_c_mini.Function.make ~parameters ~body_decls:prog.decls ~body_stms
        ()
    in
    Ac.C_named.make func ~name

  let list_to_litmus (progs : t list) ~(vars : Var.Map.t) :
      Act_c_mini.Litmus.Lang.Program.t list =
    (* We need to filter _before_ we map, since otherwise we'll end up
       assigning the wrong thread IDs. *)
    progs
    |> List.filter ~f:has_statements
    |> List.mapi ~f:(fun id prog -> to_function ~vars ~id prog)
end

module Test = struct
  type t = (Act_c_mini.Constant.t, Thread.t) Act_litmus.Test.Raw.t
  [@@deriving sexp]

  let add_new_thread : t -> t =
    Act_litmus.Test.Raw.add_thread_at_end ~thread:Thread.empty

  let threads_of_litmus (test : Act_c_mini.Litmus.Test.t) : Thread.t list =
    test |> Act_c_mini.Litmus.Test.threads
    |> List.map ~f:(Fn.compose Thread.of_function Ac.C_named.value)

  let of_litmus (test : Act_c_mini.Litmus.Test.t) : t =
    Act_litmus.Test.Raw.make
      ~header:(Act_c_mini.Litmus.Test.header test)
      ~threads:(threads_of_litmus test)

  let to_litmus (subject : t) ~(vars : Var.Map.t) :
      Act_c_mini.Litmus.Test.t Or_error.t =
    let header = Act_litmus.Test.Raw.header subject in
    let threads = Act_litmus.Test.Raw.threads subject in
    let threads' = Thread.list_to_litmus ~vars threads in
    Act_c_mini.Litmus.Test.make ~header ~threads:threads'

  let at_least_one_thread_with (p : t) ~(f : Thread.t -> bool) : bool =
    List.exists (Act_litmus.Test.Raw.threads p) ~f

  let has_atomic_statements : t -> bool =
    at_least_one_thread_with ~f:Thread.has_atomic_statements

  let has_statements : t -> bool =
    at_least_one_thread_with ~f:Thread.has_statements

  let has_if_statements : t -> bool =
    at_least_one_thread_with ~f:Thread.has_if_statements

  let has_non_label_prims : t -> bool =
    at_least_one_thread_with ~f:Thread.has_non_label_prims

  let has_while_loops : t -> bool =
    at_least_one_thread_with ~f:Thread.has_while_loops

  let has_dead_code_blocks : t -> bool =
    at_least_one_thread_with ~f:Thread.has_dead_code_blocks

  let add_var_to_init (subject : t) (name : Ac.C_id.t)
      (initial_value : Act_c_mini.Constant.t) : t Or_error.t =
    Act_litmus.Test.Raw.try_map_header subject
      ~f:(Act_litmus.Header.add_global ~name ~initial_value)

  let add_var_to_thread (subject : t) (ty : Act_c_mini.Type.t) (index : int)
      (name : Ac.C_id.t) (value : Act_c_mini.Constant.t) : t Or_error.t =
    Act_litmus.Test.Raw.try_map_thread subject ~index ~f:(fun thread ->
        Ok (Thread.add_decl thread ~ty ~name ~value))

  let declare_var (subject : t) (ty : Act_c_mini.Type.t)
      (var : Ac.Litmus_id.t) (initial_value : Act_c_mini.Constant.t) :
      t Or_error.t =
    let name = Ac.Litmus_id.variable_name var in
    match Ac.Litmus_id.tid var with
    | None ->
        add_var_to_init subject name initial_value
    | Some i ->
        add_var_to_thread subject ty i name initial_value
end
