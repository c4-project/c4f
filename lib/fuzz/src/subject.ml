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
  module Fir = Act_fir
  module Tx = Travesty_base_exts
end

module Statement = struct
  type t = Metadata.t Fir.Statement.t [@@deriving sexp]

  module If = struct
    type t = Metadata.t Fir.Statement.If.t [@@deriving sexp]
  end

  module Flow = struct
    type t = Metadata.t Fir.Statement.Flow_block.t [@@deriving sexp]
  end

  include Fir.Statement_traverse.With_meta (Metadata)

  let make_generated_prim : Fir.Prim_statement.t -> t =
    Fir.Statement.prim Metadata.generated
end

module Block = struct
  type t = (Metadata.t, Statement.t) Fir.Block.t

  let make_existing ?(statements : Statement.t list option) () : t =
    Fir.Block.make ?statements ~metadata:Metadata.existing ()

  let make_generated ?(statements : Statement.t list option) () : t =
    Fir.Block.make ?statements ~metadata:Metadata.generated ()

  let make_dead_code ?(statements : Statement.t list option) () : t =
    Fir.Block.make ?statements ~metadata:Metadata.dead_code ()
end

module Thread = struct
  type t =
    { decls: Fir.Initialiser.t Ac.C_named.Alist.t [@default []]
    ; stms: Statement.t list }
  [@@deriving sexp, make]

  let empty : t = {decls= []; stms= []}

  module An = Ac.C_named.Alist.As_named (Fir.Initialiser)

  let map_decls (thread : t)
      ~(f : Fir.Initialiser.t Ac.C_named.t -> Fir.Initialiser.t Ac.C_named.t)
      : t =
    {thread with decls= An.map ~f thread.decls}

  let add_decl (thread : t) ~(name : Ac.C_id.t) ~(init : Fir.Initialiser.t) :
      t =
    let decls' = (name, init) :: thread.decls in
    {thread with decls= decls'}

  let has_statements (p : t) : bool = not (List.is_empty p.stms)

  let exists_top_statement (p : t) ~(f : Statement.t -> bool) =
    List.exists p.stms ~f

  let statements_of_function (func : unit Fir.Function.t) : Statement.t list
      =
    func |> Fir.Function.body_stms
    |> List.map
         ~f:
           (Fir.Statement_traverse.On_meta.map ~f:(fun () ->
                Metadata.existing))

  let of_function (func : unit Fir.Function.t) : t =
    make
      ~decls:(Fir.Function.body_decls func)
      ~stms:(statements_of_function func)
      ()

  module R_alist = Ac.C_named.Alist.As_named (Var.Record)

  (** [make_function_parameters vars] creates a uniform function parameter
      list for a C litmus test using the global variable records in [vars]. *)
  let make_function_parameters (vars : Var.Map.t) :
      Fir.Type.t Ac.C_named.Alist.t =
    vars
    |> Var.Map.env_satisfying_all ~scope:Global ~predicates:[]
    |> Fir.Env.typing |> Map.to_alist

  let to_function (prog : t) ~(vars : Var.Map.t) ~(id : int) :
      unit Fir.Function.t Ac.C_named.t =
    let name = Ac.C_id.of_string (Printf.sprintf "P%d" id) in
    let body_stms =
      List.map prog.stms ~f:Fir.Statement_traverse.erase_meta
    in
    let parameters = make_function_parameters vars in
    let func =
      Fir.Function.make ~parameters ~body_decls:prog.decls ~body_stms ()
    in
    Ac.C_named.make func ~name

  let list_to_litmus (progs : t list) ~(vars : Var.Map.t) :
      Fir.Litmus.Lang.Program.t list =
    (* We need to filter _before_ we map, since otherwise we'll end up
       assigning the wrong thread IDs. *)
    progs
    |> List.filter ~f:has_statements
    |> List.mapi ~f:(fun id prog -> to_function ~vars ~id prog)
end

module Test = struct
  type t = (Fir.Constant.t, Thread.t) Act_litmus.Test.Raw.t [@@deriving sexp]

  let add_new_thread : t -> t =
    Act_litmus.Test.Raw.add_thread_at_end ~thread:Thread.empty

  let threads_of_litmus (test : Fir.Litmus.Test.t) : Thread.t list =
    test |> Fir.Litmus.Test.threads
    |> List.map ~f:(Fn.compose Thread.of_function Ac.C_named.value)

  let of_litmus (test : Fir.Litmus.Test.t) : t =
    Act_litmus.Test.Raw.make
      ~header:(Fir.Litmus.Test.header test)
      ~threads:(threads_of_litmus test)

  let to_litmus (subject : t) ~(vars : Var.Map.t) :
      Fir.Litmus.Test.t Or_error.t =
    let header = Act_litmus.Test.Raw.header subject in
    let threads = Act_litmus.Test.Raw.threads subject in
    let threads' = Thread.list_to_litmus ~vars threads in
    Fir.Litmus.Test.make ~header ~threads:threads'

  let exists_thread (p : t) ~(f : Thread.t -> bool) : bool =
    List.exists (Act_litmus.Test.Raw.threads p) ~f

  let exists_top_statement (p : t) ~(f : Statement.t -> bool) : bool =
    exists_thread p ~f:(Thread.exists_top_statement ~f)

  let has_statements ?(matching : Fir.Statement_class.t list = []) (p : t) :
      bool =
    if List.is_empty matching then exists_thread p ~f:Thread.has_statements
    else
      exists_top_statement p
        ~f:(Fir.Statement_class.rec_matches_any ~templates:matching)

  let has_statements_not_matching (p : t)
      ~(one_of : Fir.Statement_class.t list) : bool =
    exists_top_statement p
      ~f:(Fir.Statement_class.rec_unmatches_any ~templates:one_of)

  let add_var_to_init (subject : t) (name : Ac.C_id.t)
      (init : Fir.Initialiser.t) : t Or_error.t =
    match Fir.Initialiser.value init with
    | Some initial_value ->
        Act_litmus.Test.Raw.try_map_header subject
          ~f:(Act_litmus.Header.add_global ~name ~initial_value)
    | None ->
        Or_error.error_s
          [%message
            "Can't add global variable without initial value"
              ~name:(name : Ac.C_id.t)]

  let add_var_to_thread (subject : t) (index : int) (name : Ac.C_id.t)
      (init : Fir.Initialiser.t) : t Or_error.t =
    Act_litmus.Test.Raw.try_map_thread subject ~index ~f:(fun thread ->
        Ok (Thread.add_decl thread ~name ~init))

  let declare_var (subject : t) (var : Ac.Litmus_id.t)
      (init : Fir.Initialiser.t) : t Or_error.t =
    let name = Ac.Litmus_id.variable_name var in
    match Ac.Litmus_id.tid var with
    | None ->
        add_var_to_init subject name init
    | Some i ->
        add_var_to_thread subject i name init
end
