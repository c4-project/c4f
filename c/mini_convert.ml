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
open Mini

let map_combine
    (xs : 'a list) ~(f : 'a -> 'b Or_error.t) : 'b list Or_error.t =
  xs
  |> List.map ~f
  |> Or_error.combine_errors
;;

(** [sift_decls maybe_decl_list] tries to separate [maybe_decl_list]
    into a list of declarations followed immediately by a list of
    code, C89-style. *)
let sift_decls (maybe_decl_list : ([> `Decl of 'd ] as 'a) list)
  : ('d list * ('a list)) Or_error.t =
  Or_error.(
    Travesty.T_list.With_errors.fold_m maybe_decl_list
      ~init:(Fqueue.empty, Fqueue.empty)
      ~f:(fun (decls, rest) -> function
          | `Decl d ->
            if Fqueue.is_empty rest
            then return (Fqueue.enqueue decls d, rest)
            else error_string
                "Declarations must go before code."
          | item -> return (decls, Fqueue.enqueue rest item)
        )
    >>| fun (decls, rest) -> (Fqueue.to_list decls, Fqueue.to_list rest)
  )
;;

let%expect_test "sift_decls: mixed example" =
  let result =
    Or_error.(
      [ `Decl "foo"
      ; `Decl "bar"
      ; `Ndecl "baz"
      ; `Ndecl "barbaz"
      ]
      |> sift_decls
      >>| Tuple2.map_snd
        ~f:(List.map ~f:(function `Decl _ -> "DECL" | `Ndecl x -> x))
    )
  in
  Sexp.output_hum Stdio.stdout
    [%sexp (result : (string list * string list) Or_error.t)];
  [%expect {| (Ok ((foo bar) (baz barbaz))) |}]
;;

(** [ensure_functions xs] makes sure that each member of [xs] is a
    function definition. *)
let ensure_functions
  : Ast.External_decl.t list
    -> Ast.Function_def.t list Or_error.t =
  map_combine
    ~f:(
      function
      | `Fun f -> Or_error.return f
      | d      -> Or_error.error_s
                    [%message "Expected a function"
                        ~got:(d : Ast.External_decl.t) ]
    )
;;

(** [ensure_statements xs] makes sure that each member of [xs] is a
    statement. *)
let ensure_statements
  : Ast.Compound_stm.Elt.t list
    -> Ast.Stm.t list Or_error.t =
  map_combine
    ~f:(
      function
      | `Stm f -> Or_error.return f
      | d      -> Or_error.error_s
                    [%message "Expected a statement"
                        ~got:(d : Ast.Compound_stm.Elt.t) ]
    )
;;

let defined_types : (C_identifier.t, Type.Basic.t) List.Assoc.t Lazy.t =
  lazy
    [ C_identifier.of_string "atomic_int", Type.Basic.atomic_int
    ; C_identifier.of_string "bool"      , Type.Basic.bool
    ]
;;

let qualifiers_to_basic_type (quals : [> Ast.Decl_spec.t ] list)
  : Type.Basic.t Or_error.t =
  let open Or_error.Let_syntax in
  match%bind Travesty.T_list.one quals with
  | `Int -> return Type.Basic.int
  | `Defined_type t ->
    t
    |> List.Assoc.find ~equal:C_identifier.equal
      (Lazy.force defined_types)
    |> Result.of_option
      ~error:(Error.create_s
                [%message "Unknown defined type" ~got:(t : C_identifier.t)])
  | #Ast.Type_spec.t as spec ->
    Or_error.error_s
      [%message "This type isn't supported (yet)"
          ~got:(spec : Ast.Type_spec.t)]
  | #Type_qual.t as qual ->
    Or_error.error_s
      [%message "This type qualifier isn't supported (yet)"
          ~got:(qual : Type_qual.t)]
  | #Storage_class_spec.t as spec ->
    Or_error.error_s
      [%message "This storage-class specifier isn't supported (yet)"
          ~got:(spec : Storage_class_spec.t)]
;;

let declarator_to_id : Ast.Declarator.t ->
  (Identifier.t * bool) Or_error.t = function
  | { pointer = Some [[]];
      direct = Id id } ->
    Or_error.return (id, true)
  | { pointer = Some _; _ } as decl ->
    Or_error.error_s
      [%message "Complex pointers not supported yet"
          ~declarator:(decl : Ast.Declarator.t)
      ]
  | { pointer = None;
      direct  = Id id } ->
    Or_error.return (id, false)
  | x ->
    Or_error.error_s
      [%message "Unsupported direct declarator"
          ~got:(x.direct : Ast.Direct_declarator.t)
      ]
;;

let value_of_initialiser
  : Ast.Initialiser.t -> Constant.t Or_error.t = function
  | Assign (Constant v) -> Or_error.return v
  | Assign x ->
    Or_error.error_s
      [%message "Expression not supported (must be constant)"
          (x : Ast.Expr.t)]
  | List   _ ->
    Or_error.error_string "List initialisers not supported"
;;

(** [decl d] translates a declaration into an identifier-initialiser
    pair. *)
let decl (d : Ast.Decl.t)
  : (Identifier.t * Initialiser.t) Or_error.t =
  let open Or_error.Let_syntax in
  let%bind basic_type         = qualifiers_to_basic_type d.qualifiers in
  let%bind idecl              = Travesty.T_list.one d.declarator in
  let%bind (name, is_pointer) = declarator_to_id idecl.declarator in
  let%map  value = Travesty.T_option.With_errors.map_m idecl.initialiser
      ~f:value_of_initialiser
  in
  let ty = Type.of_basic ~is_pointer basic_type in
  (name, Initialiser.make ~ty ?value ())
;;

let validate_func_void_type (f : Ast.Function_def.t)
  : Validate.t =
  match f.decl_specs with
  | [ `Void ] -> Validate.pass
  | xs -> Validate.fail_s
            [%message "Expected 'void'"
                ~got:(xs : Ast.Decl_spec.t list)]
;;

let validate_func_no_knr : Ast.Function_def.t Validate.check =
  Validate.booltest
    (fun f -> List.is_empty f.Ast.Function_def.decls)
    ~if_false:"K&R style function definitions not supported"
;;

let validate_func : Ast.Function_def.t Validate.check =
  Validate.all
    [ validate_func_void_type
    ; validate_func_no_knr
    ]
;;

let param_decl : Ast.Param_decl.t -> Type.t named Or_error.t =
  function
  | { declarator = `Abstract _; _ } ->
    Or_error.error_string
      "Abstract parameter declarators not supported"
  | { qualifiers; declarator = `Concrete declarator } ->
    let open Or_error.Let_syntax in
    let%map basic_type       = qualifiers_to_basic_type qualifiers
    and     (id, is_pointer) = declarator_to_id declarator
    in
    let ty = Type.of_basic ~is_pointer basic_type in (id, ty)
;;

let param_type_list : Ast.Param_type_list.t ->
  Type.t id_assoc Or_error.t = function
  | { style = `Variadic; _ } ->
    Or_error.error_string "Variadic arguments not supported"
  | { style = `Normal; params } ->
    map_combine ~f:param_decl params
;;

let func_signature : Ast.Declarator.t ->
  (Identifier.t * Type.t id_assoc) Or_error.t = function
  | { pointer = Some _; _ } ->
    Or_error.error_string "Pointers not supported yet"
  | { pointer = None;
      direct  = Fun_decl (Id name, param_list) } ->
    Or_error.(
      param_list |> param_type_list >>| Tuple2.create name
    )
  | x ->
    Or_error.error_s
      [%message "Unsupported function declarator"
          ~got:(x.direct : Ast.Direct_declarator.t)
      ]
;;

let rec expr_to_lvalue
  : Ast.Expr.t -> Lvalue.t Or_error.t = function
  | Identifier id   -> Or_error.return (Lvalue.variable id)
  | Brackets expr -> expr_to_lvalue expr
  | Prefix (`Deref, expr) ->
    Or_error.(expr |> expr_to_lvalue >>| Lvalue.deref)
  | Prefix _ | Postfix _ | Binary _ | Ternary _ | Cast _
  | Call _ | Subscript _ | Field _ | Sizeof_type _ | String _ | Constant _
  as e ->
    Or_error.error_s
      [%message "Expected an lvalue here" ~got:(e : Ast.Expr.t)]
;;

let rec expr_to_address
  : Ast.Expr.t -> Address.t Or_error.t = function
  | Prefix (`Ref, expr) ->
    Or_error.(expr |> expr_to_address >>| Address.ref)
  | expr ->
    Or_error.(expr |> expr_to_lvalue >>| Address.lvalue)
;;

let expr_to_identifier
    (expr : Ast.Expr.t) : Identifier.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind lv = expr_to_lvalue expr in
  if Lvalue.is_deref lv
  then
    Or_error.error_s
      [%message "Expected identifier" ~got:(lv : Lvalue.t)]
  else return (Lvalue.underlying_variable lv)
;;

let expr_to_memory_order (expr : Ast.Expr.t) : Mem_order.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind id = expr_to_identifier expr in
  id
  |> C_identifier.to_string
  |> Mem_order.of_string_option
  |> Result.of_option
    ~error:(
      Error.create_s
        [%message "Unsupported memory order" ~got:(id : Identifier.t)]
    )
;;

(** [call call_table func arguments] models a function call with
    function [func] and arguments [arguments], using the modellers
    in [call_table]. *)
let call
    (call_table : (Ast.Expr.t list -> 'a Or_error.t) id_assoc Lazy.t)
    (func : Ast.Expr.t)
    (arguments : Ast.Expr.t list)
  : 'a Or_error.t =
  let open Or_error.Let_syntax in
  let%bind func_name = expr_to_identifier func in
  let%bind call_handler =
    func_name
    |> List.Assoc.find ~equal:C_identifier.equal (Lazy.force call_table)
    |> Result.of_option
      ~error:(
        Error.create_s
          [%message "Unsupported function in expression position"
              ~got:(func_name : C_identifier.t)
          ]
      )
  in call_handler arguments
;;

let model_atomic_load_explicit
  : Ast.Expr.t list -> Expression.t Or_error.t = function
  | [ raw_src; raw_mo ] ->
    let open Or_error.Let_syntax in
    let%map src = expr_to_address raw_src
    and     mo  = expr_to_memory_order raw_mo
    in Expression.atomic_load
      (Atomic_load.make ~src ~mo)
  | args ->
    Or_error.error_s
      [%message "Invalid arguments to atomic_load_explicit"
          ~got:(args : Ast.Expr.t list)
      ]
;;

let expr_call_table
  : (Ast.Expr.t list -> Expression.t Or_error.t) id_assoc Lazy.t =
  lazy
    [ C_identifier.of_string "atomic_load_explicit", model_atomic_load_explicit
    ]
;;

let rec expr
  : Ast.Expr.t -> Expression.t Or_error.t =
  let open Or_error.Let_syntax in
  let model_binary l op r =
    let%bind l' = expr l
    and      r' = expr r
    in
    match op with
    | `Eq -> return (Expression.eq l' r')
    | _   -> Or_error.error_s
               [%message "Unsupported binary operator" ~got:(op : Operators.Bin.t)]
  in
  function
  | Brackets e -> expr e
  | Binary (l, op, r) -> model_binary l op r
  | Constant k -> Or_error.return (Expression.constant k)
  | Identifier id ->
    Or_error.return (Expression.lvalue (Lvalue.variable id))
  | Prefix (`Deref, expr) ->
    Or_error.(expr |> expr_to_lvalue >>| Lvalue.deref >>| Expression.lvalue)
  | Call { func; arguments } -> call expr_call_table func arguments
  | Prefix _ | Postfix _ | Ternary _ | Cast _
  | Subscript _ | Field _ | Sizeof_type _ | String _
  as e ->
    Or_error.error_s
      [%message "Unsupported expression" ~got:(e : Ast.Expr.t)]
;;

let%expect_test "model atomic_load_explicit" =
  Sexp.output_hum Stdio.stdout
    [%sexp
      (expr
         Ast.(
           Expr.Call { func = Identifier
                           (C_identifier.of_string "atomic_load_explicit")
                     ; arguments =
                         [ Prefix (`Ref, Identifier (C_identifier.of_string "x"))
                         ; Identifier
                             (C_identifier.of_string "memory_order_seq_cst")
                         ]
                     }
         )
       : Expression.t Or_error.t )
    ];
  [%expect {|
      (Ok
       (Atomic_load ((src (Ref (Lvalue (Variable x)))) (mo memory_order_seq_cst)))) |}]
;;

let model_atomic_store
  : Ast.Expr.t list -> Statement.t Or_error.t = function
  | [ raw_dst; raw_src; raw_mo ] ->
    let open Or_error.Let_syntax in
    let%map dst = expr_to_address raw_dst
    and     src = expr raw_src
    and     mo  = expr_to_memory_order raw_mo
    in Statement.atomic_store
      (Atomic_store.make ~dst ~src ~mo)
  | args ->
    Or_error.error_s
      [%message "Invalid arguments to atomic_store_explicit"
          ~got:(args : Ast.Expr.t list)
      ]
;;

let model_atomic_cmpxchg
  : Ast.Expr.t list -> Statement.t Or_error.t = function
  | [ raw_obj; raw_expected; raw_desired; raw_succ; raw_fail ] ->
    let open Or_error.Let_syntax in
    let%map obj      = expr_to_address      raw_obj      (* volatile A* *)
    and     expected = expr_to_address      raw_expected (* C* *)
    and     desired  = expr                 raw_desired  (* C *)
    and     succ     = expr_to_memory_order raw_succ     (* memory_order *)
    and     fail     = expr_to_memory_order raw_fail     (* memory_order *)
    in Statement.atomic_cmpxchg
      (Atomic_cmpxchg.make ~obj ~expected ~desired ~succ ~fail)
  | args ->
    Or_error.error_s
      [%message
        "Invalid arguments to atomic_compare_exchange_strong_explicit"
          ~got:(args : Ast.Expr.t list)
      ]
;;

let expr_stm_call_table
  : (Ast.Expr.t list -> Statement.t Or_error.t) id_assoc Lazy.t =
  lazy
    [ C_identifier.of_string "atomic_store_explicit", model_atomic_store
    ; C_identifier.of_string "atomic_compare_exchange_strong_explicit", model_atomic_cmpxchg
    ]
;;

let expr_stm : Ast.Expr.t -> Statement.t Or_error.t = function
  | Binary (l, `Assign, r) ->
    let open Or_error.Let_syntax in
    let%map lvalue = expr_to_lvalue l
    and     rvalue = expr r
    in Statement.assign (Assign.make ~lvalue ~rvalue)
  | Call { func; arguments } -> call expr_stm_call_table func arguments
  | Brackets _ | Constant _
  | Prefix _ | Postfix _ | Binary _ | Ternary _ | Cast _
  | Subscript _ | Field _ | Sizeof_type _ | String _ | Identifier _
  as e ->
    Or_error.error_s
      [%message "Unsupported expression statement" ~got:(e : Ast.Expr.t)]
;;

let possible_compound_to_list
  : Ast.Stm.t -> Ast.Stm.t list Or_error.t = function
  | Ast.Stm.Compound elems ->
    let open Or_error.Let_syntax in
    let%bind (_, ast_nondecls) = sift_decls elems in
    ensure_statements ast_nondecls
  | stm -> Or_error.return [ stm ]
;;

let model_if
    (model_stm : Ast.Stm.t -> Statement.t Or_error.t)
    (old_cond : Ast.Expr.t)
    (old_t_branch : Ast.Stm.t) (old_f_branch : Ast.Stm.t option)
  : Statement.t Or_error.t =
  let open Or_error.Let_syntax in
    let%bind cond = expr old_cond in
    let%bind t_list = possible_compound_to_list old_t_branch in
    let%bind f_list =
      Option.value_map ~f:possible_compound_to_list old_f_branch
        ~default:(Or_error.return [])
    in
    let model_if_branch xs =
      xs |> List.map ~f:model_stm |> Or_error.combine_errors
    in
    let%map t_branch = model_if_branch t_list
    and     f_branch = model_if_branch f_list
    in Statement.if_stm ~cond ~t_branch ~f_branch ()
;;

let rec stm : Ast.Stm.t -> Statement.t Or_error.t =
  function
  | Expr None -> Or_error.return Statement.nop
  | Expr (Some e) -> expr_stm e
  | If { cond; t_branch; f_branch } ->
    model_if stm cond t_branch f_branch
  | Continue | Break | Return _ | Label _ | Compound _
  | Switch _ | While _ | Do_while _ | For _ | Goto _
  as s ->
    Or_error.error_s
      [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]
;;

let%expect_test "model atomic_store_explicit" =
  Sexp.output_hum Stdio.stdout
    [%sexp
      (stm
         Ast.(
           Stm.Expr
             (Some
                (Expr.Call
                   { func = Identifier (C_identifier.of_string "atomic_store_explicit")
                   ; arguments =
                       [ Prefix (`Ref, Identifier (C_identifier.of_string "x"))
                       ; Constant (Integer 42)
                       ; Identifier (C_identifier.of_string "memory_order_relaxed")
                       ]
                   }
                )
             )
         )
       : Statement.t Or_error.t )
    ];
  [%expect {|
      (Ok
       (Atomic_store
        ((src (Constant (Integer 42))) (dst (Ref (Lvalue (Variable x))))
         (mo memory_order_relaxed)))) |}]
;;

let%expect_test "model atomic cmpxchg" =
  Sexp.output_hum Stdio.stdout
    [%sexp
      (stm
         Ast.(
           Stm.Expr
             (Some
                (Expr.Call
                   { func = Identifier (C_identifier.of_string "atomic_compare_exchange_strong_explicit")
                   ; arguments =
                       [ Prefix (`Ref, Identifier (C_identifier.of_string "x"))
                       ; Prefix (`Ref, Identifier (C_identifier.of_string "y"))
                       ; Constant (Integer 42)
                       ; Identifier (C_identifier.of_string "memory_order_relaxed")
                       ; Identifier (C_identifier.of_string "memory_order_relaxed")
                       ]
                   }
                )
             )
         )
       : Statement.t Or_error.t )
    ];
  [%expect {|
      (Ok
       (Atomic_cmpxchg
        ((obj (Ref (Lvalue (Variable x)))) (expected (Ref (Lvalue (Variable y))))
         (desired (Constant (Integer 42))) (succ memory_order_relaxed)
         (fail memory_order_relaxed)))) |}]
;;


let func_body (body : Ast.Compound_stm.t)
  : (Initialiser.t id_assoc * Statement.t list) Or_error.t =
  let open Or_error.Let_syntax in
  let%bind (ast_decls, ast_nondecls) = sift_decls body in
  let%bind ast_stms = ensure_statements ast_nondecls in
  let%map  decls = map_combine ~f:decl ast_decls
  and      stms  = map_combine ~f:stm  ast_stms
  in (decls, stms)
;;

let func (f : Ast.Function_def.t)
  : (Identifier.t * Function.t) Or_error.t =
  let open Or_error.Let_syntax in
  let%bind () = Validate.result (validate_func f) in
  let%map  (name, parameters)      = func_signature f.signature
  and      (body_decls, body_stms) = func_body f.body
  in (name, Function.make ~parameters ~body_decls ~body_stms ())
;;

let translation_unit (prog : Ast.Translation_unit.t) : Program.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind (ast_decls, ast_nondecls) = sift_decls prog in
  let%bind ast_funs = ensure_functions ast_nondecls in
  let%map  globals   = map_combine ~f:decl ast_decls
  and      functions = map_combine ~f:func ast_funs
  in Program.make ~globals ~functions
;;

module Litmus_conv = Litmus.Ast.Convert (struct
    module From = struct
      include Ast.Litmus
      module Lang = Ast.Litmus_lang
    end
    module To = Mini_litmus.Ast

    let program = func
    let constant = Or_error.return
  end)
;;

let litmus
  : Ast.Litmus.Validated.t
    -> Mini_litmus.Ast.Validated.t Or_error.t =
  Litmus_conv.convert
;;
