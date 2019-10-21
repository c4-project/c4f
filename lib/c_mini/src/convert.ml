(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Fqueue *)

module Ac = Act_common
module Tx = Travesty_base_exts
module Ast = Act_c_lang.Ast

(** [sift_decls maybe_decl_list] tries to separate [maybe_decl_list] into a
    list of declarations followed immediately by a list of code, C89-style. *)
let sift_decls (maybe_decl_list : ([> `Decl of 'd] as 'a) list) :
    ('d list * 'a list) Or_error.t =
  Or_error.(
    Tx.List.With_errors.fold_m maybe_decl_list
      ~init:(Fqueue.empty, Fqueue.empty) ~f:(fun (decls, rest) ->
      function
      | `Decl d ->
          if Fqueue.is_empty rest then return (Fqueue.enqueue decls d, rest)
          else error_string "Declarations must go before code."
      | item ->
          return (decls, Fqueue.enqueue rest item))
    >>| fun (decls, rest) -> (Fqueue.to_list decls, Fqueue.to_list rest))

let%expect_test "sift_decls: mixed example" =
  let result =
    Or_error.(
      [`Decl "foo"; `Decl "bar"; `Ndecl "baz"; `Ndecl "barbaz"]
      |> sift_decls
      >>| Tuple2.map_snd
            ~f:(List.map ~f:(function `Decl _ -> "DECL" | `Ndecl x -> x)))
  in
  Stdio.print_s [%sexp (result : (string list * string list) Or_error.t)] ;
  [%expect {| (Ok ((foo bar) (baz barbaz))) |}]

(** [ensure_functions xs] makes sure that each member of [xs] is a function
    definition. *)
let ensure_functions :
    Ast.External_decl.t list -> Ast.Function_def.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Fun f ->
        Or_error.return f
    | d ->
        Or_error.error_s
          [%message "Expected a function" ~got:(d : Ast.External_decl.t)])

(** [ensure_statements xs] makes sure that each member of [xs] is a
    statement. *)
let ensure_statements :
    Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Stm f ->
        Or_error.return f
    | d ->
        Or_error.error_s
          [%message
            "Expected a statement" ~got:(d : Ast.Compound_stm.Elt.t)])

let defined_types : (Ac.C_id.t, Type.Basic.t) List.Assoc.t Lazy.t =
  lazy
    Type.Basic.
      [ (Ac.C_id.of_string "atomic_bool", bool ~atomic:true ())
      ; (Ac.C_id.of_string "atomic_int", int ~atomic:true ())
      ; (Ac.C_id.of_string "bool", bool ()) ]

let qualifiers_to_basic_type (quals : [> Ast.Decl_spec.t] list) :
    Type.Basic.t Or_error.t =
  let open Or_error.Let_syntax in
  match%bind Tx.List.one quals with
  | `Int ->
      return (Type.Basic.int ())
  | `Defined_type t ->
      t
      |> List.Assoc.find ~equal:Ac.C_id.equal (Lazy.force defined_types)
      |> Result.of_option
           ~error:
             (Error.create_s
                [%message "Unknown defined type" ~got:(t : Ac.C_id.t)])
  | #Ast.Type_spec.t as spec ->
      Or_error.error_s
        [%message
          "This type isn't supported (yet)" ~got:(spec : Ast.Type_spec.t)]
  | #Act_c_lang.Ast_basic.Type_qual.t as qual ->
      Or_error.error_s
        [%message
          "This type qualifier isn't supported (yet)"
            ~got:(qual : Act_c_lang.Ast_basic.Type_qual.t)]
  | #Act_c_lang.Ast_basic.Storage_class_spec.t as spec ->
      Or_error.error_s
        [%message
          "This storage-class specifier isn't supported (yet)"
            ~got:(spec : Act_c_lang.Ast_basic.Storage_class_spec.t)]

let declarator_to_id :
    Ast.Declarator.t -> (Act_common.C_id.t * bool) Or_error.t = function
  | {pointer= Some [[]]; direct= Id id} ->
      Or_error.return (id, true)
  | {pointer= Some _; _} as decl ->
      Or_error.error_s
        [%message
          "Complex pointers not supported yet"
            ~declarator:(decl : Ast.Declarator.t)]
  | {pointer= None; direct= Id id} ->
      Or_error.return (id, false)
  | x ->
      Or_error.error_s
        [%message
          "Unsupported direct declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)]

let constant : Act_c_lang.Ast_basic.Constant.t -> Constant.t Or_error.t =
  function
  | Integer k ->
      Or_error.return (Constant.int k)
  | Char _ | Float _ ->
      Or_error.error_string "Unsupported constant type"

let value_of_initialiser : Ast.Initialiser.t -> Constant.t Or_error.t =
  function
  | Assign (Constant v) ->
      (* TODO(@MattWindsor91): Boolean initialisers aren't covered by this
         case, as C99 Boolean 'constant's are identifiers. *)
      constant v
  | Assign x ->
      Or_error.error_s
        [%message
          "Expression not supported (must be constant)" (x : Ast.Expr.t)]
  | List _ ->
      Or_error.error_string "List initialisers not supported"

(** [decl d] translates a declaration into an identifier-initialiser pair. *)
let decl (d : Ast.Decl.t) : (Act_common.C_id.t * Initialiser.t) Or_error.t =
  Or_error.Let_syntax.(
    let%bind basic_type = qualifiers_to_basic_type d.qualifiers in
    let%bind idecl = Tx.List.one d.declarator in
    let%bind name, pointer = declarator_to_id idecl.declarator in
    let%map value =
      Tx.Option.With_errors.map_m idecl.initialiser ~f:value_of_initialiser
    in
    let ty = Type.of_basic ~pointer basic_type in
    (name, Initialiser.make ~ty ?value ()))

let validate_func_void_type (f : Ast.Function_def.t) : Validate.t =
  match f.decl_specs with
  | [`Void] ->
      Validate.pass
  | xs ->
      Validate.fail_s
        [%message "Expected 'void'" ~got:(xs : Ast.Decl_spec.t list)]

let validate_func_no_knr : Ast.Function_def.t Validate.check =
  Validate.booltest
    (fun f -> List.is_empty f.Ast.Function_def.decls)
    ~if_false:"K&R style function definitions not supported"

let validate_func : Ast.Function_def.t Validate.check =
  Validate.all [validate_func_void_type; validate_func_no_knr]

let param_decl : Ast.Param_decl.t -> Type.t Named.t Or_error.t = function
  | {declarator= `Abstract _; _} ->
      Or_error.error_string "Abstract parameter declarators not supported"
  | {qualifiers; declarator= `Concrete declarator} ->
      let open Or_error.Let_syntax in
      let%map basic_type = qualifiers_to_basic_type qualifiers
      and name, pointer = declarator_to_id declarator in
      let value = Type.of_basic ~pointer basic_type in
      Named.make value ~name

let param_type_list :
    Ast.Param_type_list.t -> Type.t Named.Alist.t Or_error.t = function
  | {style= `Variadic; _} ->
      Or_error.error_string "Variadic arguments not supported"
  | {style= `Normal; params} ->
      Or_error.(
        params
        |> Tx.Or_error.combine_map ~f:param_decl
        >>| Named.alist_of_list)

let func_signature :
       Ast.Declarator.t
    -> (Act_common.C_id.t * Type.t Named.Alist.t) Or_error.t = function
  | {pointer= Some _; _} ->
      Or_error.error_string "Pointers not supported yet"
  | {pointer= None; direct= Fun_decl (Id name, param_list)} ->
      Or_error.(param_list |> param_type_list >>| Tuple2.create name)
  | x ->
      Or_error.error_s
        [%message
          "Unsupported function declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)]

let rec expr_to_lvalue : Ast.Expr.t -> Lvalue.t Or_error.t = function
  | Identifier id ->
      Or_error.return (Lvalue.variable id)
  | Brackets expr ->
      expr_to_lvalue expr
  | Prefix (`Deref, expr) ->
      Or_error.(expr |> expr_to_lvalue >>| Lvalue.deref)
  | ( Prefix _
    | Postfix _
    | Binary _
    | Ternary _
    | Cast _
    | Call _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _
    | Constant _ ) as e ->
      Or_error.error_s
        [%message "Expected an lvalue here" ~got:(e : Ast.Expr.t)]

let rec expr_to_address : Ast.Expr.t -> Address.t Or_error.t = function
  | Prefix (`Ref, expr) ->
      Or_error.(expr |> expr_to_address >>| Address.ref)
  | expr ->
      Or_error.(expr |> expr_to_lvalue >>| Address.lvalue)

let expr_to_identifier (expr : Ast.Expr.t) : Act_common.C_id.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind lv = expr_to_lvalue expr in
  if Lvalue.is_deref lv then
    Or_error.error_s [%message "Expected identifier" ~got:(lv : Lvalue.t)]
  else return (Lvalue.variable_of lv)

let expr_to_memory_order (expr : Ast.Expr.t) : Mem_order.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind id = expr_to_identifier expr in
  id |> Ac.C_id.to_string |> Mem_order.of_string_option
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message
              "Unsupported memory order" ~got:(id : Act_common.C_id.t)])

(** [call call_table func arguments] models a function call with function
    [func] and arguments [arguments], using the modellers in [call_table]. *)
let call
    (call_table : (Ast.Expr.t list -> 'a Or_error.t) Named.Alist.t Lazy.t)
    (func : Ast.Expr.t) (arguments : Ast.Expr.t list) : 'a Or_error.t =
  let open Or_error.Let_syntax in
  let%bind func_name = expr_to_identifier func in
  let%bind call_handler =
    func_name
    |> List.Assoc.find ~equal:Ac.C_id.equal (Lazy.force call_table)
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Unsupported function in expression position"
                  ~got:(func_name : Ac.C_id.t)])
  in
  call_handler arguments

let model_atomic_load_explicit : Ast.Expr.t list -> Expression.t Or_error.t
    = function
  | [raw_src; raw_mo] ->
      let open Or_error.Let_syntax in
      let%map src = expr_to_address raw_src
      and mo = expr_to_memory_order raw_mo in
      Expression.atomic_load (Atomic_load.make ~src ~mo)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_load_explicit"
            ~got:(args : Ast.Expr.t list)]

let expr_call_table :
    (Ast.Expr.t list -> Expression.t Or_error.t) Named.Alist.t Lazy.t =
  lazy
    [(Ac.C_id.of_string "atomic_load_explicit", model_atomic_load_explicit)]

let identifier_to_expr (id : Ac.C_id.t) : Expression.t =
  match Ac.C_id.to_string id with
  | "true" ->
      Expression.bool_lit true
  | "false" ->
      Expression.bool_lit false
  | _ ->
      Expression.lvalue (Lvalue.variable id)

let rec expr : Ast.Expr.t -> Expression.t Or_error.t =
  let open Or_error.Let_syntax in
  let model_binary l op r =
    let%bind l' = expr l and r' = expr r in
    match op with
    | `Eq ->
        return (Expression.eq l' r')
    | _ ->
        Or_error.error_s
          [%message
            "Unsupported binary operator"
              ~got:(op : Act_c_lang.Ast_basic.Operators.Bin.t)]
  in
  function
  | Brackets e ->
      expr e
  | Binary (l, op, r) ->
      model_binary l op r
  | Constant k ->
      Or_error.map ~f:Expression.constant (constant k)
  | Identifier id ->
      Or_error.return (identifier_to_expr id)
  | Prefix (`Deref, expr) ->
      Or_error.(
        expr |> expr_to_lvalue >>| Lvalue.deref >>| Expression.lvalue)
  | Call {func; arguments} ->
      call expr_call_table func arguments
  | ( Prefix _
    | Postfix _
    | Ternary _
    | Cast _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _ ) as e ->
      Or_error.error_s
        [%message "Unsupported expression" ~got:(e : Ast.Expr.t)]

let%expect_test "model atomic_load_explicit" =
  Stdio.print_s
    [%sexp
      ( expr
          Ast.(
            Expr.Call
              { func= Identifier (Ac.C_id.of_string "atomic_load_explicit")
              ; arguments=
                  [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                  ; Identifier (Ac.C_id.of_string "memory_order_seq_cst") ]
              })
        : Expression.t Or_error.t )] ;
  [%expect
    {|
      (Ok
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_seq_cst)))) |}]

let model_atomic_store : Ast.Expr.t list -> unit Statement.t Or_error.t =
  function
  | [raw_dst; raw_src; raw_mo] ->
      let open Or_error.Let_syntax in
      let%map dst = expr_to_address raw_dst
      and src = expr raw_src
      and mo = expr_to_memory_order raw_mo in
      Statement.atomic_store (Atomic_store.make ~dst ~src ~mo)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_store_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_atomic_cmpxchg : Ast.Expr.t list -> unit Statement.t Or_error.t =
  function
  | [raw_obj; raw_expected; raw_desired; raw_succ; raw_fail] ->
      let open Or_error.Let_syntax in
      let%map obj = expr_to_address raw_obj (* volatile A* *)
      and expected = expr_to_address raw_expected (* C* *)
      and desired = expr raw_desired (* C *)
      and succ = expr_to_memory_order raw_succ (* memory_order *)
      and fail = expr_to_memory_order raw_fail (* memory_order *) in
      Statement.atomic_cmpxchg
        (Atomic_cmpxchg.make ~obj ~expected ~desired ~succ ~fail)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_compare_exchange_strong_explicit"
            ~got:(args : Ast.Expr.t list)]

let expr_stm_call_table :
    (Ast.Expr.t list -> unit Statement.t Or_error.t) Named.Alist.t Lazy.t =
  lazy
    [ (Ac.C_id.of_string "atomic_store_explicit", model_atomic_store)
    ; ( Ac.C_id.of_string "atomic_compare_exchange_strong_explicit"
      , model_atomic_cmpxchg ) ]

let expr_stm : Ast.Expr.t -> unit Statement.t Or_error.t = function
  | Binary (l, `Assign, r) ->
      let open Or_error.Let_syntax in
      let%map lvalue = expr_to_lvalue l and rvalue = expr r in
      Statement.assign (Assign.make ~lvalue ~rvalue)
  | Call {func; arguments} ->
      call expr_stm_call_table func arguments
  | ( Brackets _
    | Constant _
    | Prefix _
    | Postfix _
    | Binary _
    | Ternary _
    | Cast _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _
    | Identifier _ ) as e ->
      Or_error.error_s
        [%message "Unsupported expression statement" ~got:(e : Ast.Expr.t)]

let possible_compound_to_list : Ast.Stm.t -> Ast.Stm.t list Or_error.t =
  function
  | Ast.Stm.Compound elems ->
      let open Or_error.Let_syntax in
      let%bind _, ast_nondecls = sift_decls elems in
      ensure_statements ast_nondecls
  | stm ->
      Or_error.return [stm]

let model_if_branch (model_stm : Ast.Stm.t -> unit Statement.t Or_error.t)
    (branch : Ast.Stm.t) : (unit, unit Statement.t) Block.t Or_error.t =
  Or_error.(
    branch |> possible_compound_to_list
    >>= Tx.Or_error.combine_map ~f:model_stm
    >>| Block.of_statement_list)

let model_if (model_stm : Ast.Stm.t -> unit Statement.t Or_error.t)
    (old_cond : Ast.Expr.t) (old_t_branch : Ast.Stm.t)
    (old_f_branch : Ast.Stm.t option) : unit Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind cond = expr old_cond in
    let%bind t_branch = model_if_branch model_stm old_t_branch in
    let%map f_branch =
      Option.value_map old_f_branch
        ~f:(model_if_branch model_stm)
        ~default:(Or_error.return (Block.of_statement_list []))
    in
    let ifs = Statement.If.make ~cond ~t_branch ~f_branch in
    Statement.if_stm ifs)

let rec stm : Ast.Stm.t -> unit Statement.t Or_error.t = function
  | Expr None ->
      Or_error.return (Statement.nop ())
  | Expr (Some e) ->
      expr_stm e
  | If {cond; t_branch; f_branch} ->
      model_if stm cond t_branch f_branch
  | ( Continue
    | Break
    | Return _
    | Label _
    | Compound _
    | Switch _
    | While _
    | Do_while _
    | For _
    | Goto _ ) as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]

let%expect_test "model atomic_store_explicit" =
  Stdio.print_s
    [%sexp
      ( stm
          Ast.(
            Stm.Expr
              (Some
                 (Expr.Call
                    { func=
                        Identifier
                          (Ac.C_id.of_string "atomic_store_explicit")
                    ; arguments=
                        [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                        ; Constant (Integer 42)
                        ; Identifier
                            (Ac.C_id.of_string "memory_order_relaxed") ] })))
        : unit Statement.t Or_error.t )] ;
  [%expect
    {|
      (Ok
       (Atomic_store
        ((src (Constant (Int 42))) (dst (Ref (Lvalue (Variable x))))
         (mo memory_order_relaxed)))) |}]

let%expect_test "model atomic cmpxchg" =
  Stdio.print_s
    [%sexp
      ( stm
          Ast.(
            Stm.Expr
              (Some
                 (Expr.Call
                    { func=
                        Identifier
                          (Ac.C_id.of_string
                             "atomic_compare_exchange_strong_explicit")
                    ; arguments=
                        [ Prefix (`Ref, Identifier (Ac.C_id.of_string "x"))
                        ; Prefix (`Ref, Identifier (Ac.C_id.of_string "y"))
                        ; Constant (Integer 42)
                        ; Identifier
                            (Ac.C_id.of_string "memory_order_relaxed")
                        ; Identifier
                            (Ac.C_id.of_string "memory_order_relaxed") ] })))
        : unit Statement.t Or_error.t )] ;
  [%expect
    {|
      (Ok
       (Atomic_cmpxchg
        ((obj (Ref (Lvalue (Variable x)))) (expected (Ref (Lvalue (Variable y))))
         (desired (Constant (Int 42))) (succ memory_order_relaxed)
         (fail memory_order_relaxed)))) |}]

let func_body (body : Ast.Compound_stm.t) :
    (Initialiser.t Named.Alist.t * unit Statement.t list) Or_error.t =
  let open Or_error.Let_syntax in
  let%bind ast_decls, ast_nondecls = sift_decls body in
  let%bind ast_stms = ensure_statements ast_nondecls in
  let%map decls = Tx.Or_error.combine_map ~f:decl ast_decls
  and stms = Tx.Or_error.combine_map ~f:stm ast_stms in
  (decls, stms)

let func (f : Ast.Function_def.t) : unit Function.t Named.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind () = Validate.result (validate_func f) in
    let%map name, parameters = func_signature f.signature
    and body_decls, body_stms = func_body f.body in
    Named.make ~name (Function.make ~parameters ~body_decls ~body_stms ()))

let translation_unit (prog : Ast.Translation_unit.t) :
    unit Program.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind ast_decls, ast_nondecls = sift_decls prog in
    let%bind ast_funs = ensure_functions ast_nondecls in
    let%map globals = Tx.Or_error.combine_map ~f:decl ast_decls
    and function_list = Tx.Or_error.combine_map ~f:func ast_funs in
    let functions = Named.alist_of_list function_list in
    Program.make ~globals ~functions)

module Litmus_conv = Act_litmus.Convert.Make (struct
  module From = struct
    include Ast.Litmus
    module Lang = Ast.Litmus_lang
  end

  module To = Litmus.Test

  let program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t = func

  let constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t =
    constant
end)

let litmus_post :
       Act_c_lang.Ast_basic.Constant.t Act_litmus.Postcondition.t
    -> Constant.t Act_litmus.Postcondition.t Or_error.t =
  Litmus_conv.convert_post

let litmus : Ast.Litmus.t -> Litmus.Test.t Or_error.t = Litmus_conv.convert

let litmus_of_raw_ast (ast : Act_litmus.Ast.M(Act_c_lang.Ast.Litmus_lang).t)
    =
  Or_error.(ast |> Ast.Litmus.of_ast >>= litmus)
