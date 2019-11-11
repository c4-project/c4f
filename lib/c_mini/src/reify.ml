(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ast = Act_c_lang.Ast

let bool_lit_to_expr (b : bool) : Ast.Expr.t =
  Ast.Expr.Identifier
    (Act_common.C_id.of_string (if b then "true" else "false"))

let constant_to_expr : Constant.t -> Ast.Expr.t =
  Constant.reduce
    ~int:(fun i -> Ast.Expr.Constant (Integer i))
    ~bool:bool_lit_to_expr

let to_initialiser (value : Constant.t) : Ast.Initialiser.t =
  Assign (constant_to_expr value)

let type_to_spec (ty : Type.t) : [> Act_c_lang.Ast.Type_spec.t] =
  (* We translate the level of indirection separately, in [type_to_pointer]. *)
  Type.Basic.to_spec (Type.basic_type ty)

let type_to_pointer (ty : Type.t) : Act_c_lang.Ast_basic.Pointer.t option =
  (* We translate the actual underlying type separately, in [type_to_spec]. *)
  Option.some_if (Type.is_pointer ty) [[]]

let id_declarator (ty : Type.t) (id : Act_common.C_id.t) : Ast.Declarator.t =
  {pointer= type_to_pointer ty; direct= Id id}

let decl (id : Act_common.C_id.t) (elt : Initialiser.t) : Ast.Decl.t =
  let ty = Initialiser.ty elt in
  let value = Initialiser.value elt in
  { qualifiers= [type_to_spec ty]
  ; declarator=
      [ { declarator= id_declarator ty id
        ; initialiser= Option.map ~f:to_initialiser value } ] }

let decls : Initialiser.t Named.Alist.t -> [> `Decl of Ast.Decl.t] list =
  List.map ~f:(fun (k, v) -> `Decl (decl k v))

let func_parameter (id : Act_common.C_id.t) (ty : Type.t) : Ast.Param_decl.t
    =
  {qualifiers= [type_to_spec ty]; declarator= `Concrete (id_declarator ty id)}

let func_parameters (parameters : Type.t Named.Alist.t) :
    Ast.Param_type_list.t =
  { params= List.map ~f:(Tuple2.uncurry func_parameter) parameters
  ; style= `Normal }

let func_signature (id : Act_common.C_id.t)
    (parameters : Type.t Named.Alist.t) : Ast.Declarator.t =
  {pointer= None; direct= Fun_decl (Id id, func_parameters parameters)}

let lvalue_to_expr : Lvalue.t -> Ast.Expr.t =
  Lvalue.reduce
    ~variable:(fun x -> Ast.Expr.Identifier x)
    ~deref:(fun l -> Prefix (`Deref, l))

let address_to_expr : Address.t -> Ast.Expr.t =
  Address.reduce ~lvalue:lvalue_to_expr ~ref:(fun l -> Prefix (`Ref, l))

let mem_order_to_expr (mo : Mem_order.t) : Ast.Expr.t =
  Identifier (Act_common.C_id.of_string (Mem_order.to_string mo))

let known_call (name : string) (args : Ast.Expr.t list) : Ast.Expr.t =
  Call {func= Identifier (Act_common.C_id.of_string name); arguments= args}

let atomic_load_to_expr (ld : Atomic_load.t) : Ast.Expr.t =
  known_call "atomic_load_explicit"
    [ address_to_expr (Atomic_load.src ld)
    ; mem_order_to_expr (Atomic_load.mo ld) ]

let bop : Expression.Bop.t -> Act_c_lang.Ast_basic.Operators.Bin.t = function
  | Expression.Bop.Eq ->
      `Eq
  | L_and ->
      `Land
  | L_or ->
      `Lor

(* NB: This works because all of the bops are left-associative, and will need
   refining if any right-associative Bops appear. *)
let needs_brackets (o : Act_c_lang.Ast_basic.Operators.Bin.t)
    (expr : Ast.Expr.t) ~(is_left : bool) : bool =
  match expr with
  | Binary (Brackets _, _, _) when is_left ->
      true
  | Binary (_, _, Brackets _) when not is_left ->
      true
  | Binary (_, o', _) ->
      is_left && not (Act_c_lang.Ast_basic.Operators.Bin.equal o o')
  | _ ->
      false

let bop_expr (op : Expression.Bop.t) (l : Ast.Expr.t) (r : Ast.Expr.t) :
    Ast.Expr.t =
  let op' = bop op in
  let l' =
    if needs_brackets op' l ~is_left:true then Ast.Expr.Brackets l else l
  in
  let r' =
    if needs_brackets op' r ~is_left:false then Ast.Expr.Brackets r else r
  in
  Ast.Expr.Binary (l', op', r')

let expr : Expression.t -> Ast.Expr.t =
  Expression.reduce ~constant:constant_to_expr ~lvalue:lvalue_to_expr
    ~atomic_load:atomic_load_to_expr ~bop:bop_expr

let known_call_stm (name : string) (args : Ast.Expr.t list) : Ast.Stm.t =
  Expr (Some (known_call name args))

let atomic_cmpxchg (cmpxchg : Atomic_cmpxchg.t) : Ast.Stm.t =
  known_call_stm "atomic_compare_exchange_strong_explicit"
    [ address_to_expr (Atomic_cmpxchg.obj cmpxchg)
    ; address_to_expr (Atomic_cmpxchg.expected cmpxchg)
    ; expr (Atomic_cmpxchg.desired cmpxchg)
    ; mem_order_to_expr (Atomic_cmpxchg.succ cmpxchg)
    ; mem_order_to_expr (Atomic_cmpxchg.fail cmpxchg) ]

let atomic_store (st : Atomic_store.t) : Ast.Stm.t =
  known_call_stm "atomic_store_explicit"
    [ address_to_expr (Atomic_store.dst st)
    ; expr (Atomic_store.src st)
    ; mem_order_to_expr (Atomic_store.mo st) ]

let assign (asn : Assign.t) : Ast.Stm.t =
  let lvalue = Assign.lvalue asn in
  let rvalue = Assign.rvalue asn in
  Expr (Some (Binary (lvalue_to_expr lvalue, `Assign, expr rvalue)))

let lift_stms (type stm) (stm : stm -> Ast.Stm.t) (xs : stm list) :
    Ast.Compound_stm.t =
  List.map ~f:(fun s -> `Stm (stm s)) xs

let block (type meta stm) (stm : stm -> Ast.Stm.t) (b : (meta, stm) Block.t)
    : Ast.Stm.t =
  Compound (lift_stms stm (Block.statements b))

let rec stm : _ Statement.t -> Ast.Stm.t =
  Statement.reduce ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~while_loop
    ~nop:(fun () -> Expr None)

and if_stm (ifs : _ Statement.If.t) : Ast.Stm.t =
  If
    { cond= expr (Statement.If.cond ifs)
    ; t_branch= block stm (Statement.If.t_branch ifs)
    ; f_branch=
        ( match Statement.If.f_branch ifs with
        | x when Block.is_empty x ->
            None
        | x ->
            Some (block stm x) ) }

and while_loop (loop : _ Statement.While.t) : Ast.Stm.t =
  let cond = expr (Statement.While.cond loop)
  and body = block stm (Statement.While.body loop) in
  match Statement.While.kind loop with
  | `While ->
      While (cond, body)
  | `Do_while ->
      Do_while (body, cond)

let func_body (ds : Initialiser.t Named.Alist.t)
    (statements : _ Statement.t list) : Ast.Compound_stm.t =
  decls ds @ lift_stms stm statements

let func (id : Act_common.C_id.t) (def : _ Function.t) : Ast.External_decl.t
    =
  let parameters = Function.parameters def in
  let body_decls = Function.body_decls def in
  let body_stms = Function.body_stms def in
  `Fun
    { decl_specs= [`Void]
    ; signature= func_signature id parameters
    ; decls= []
    ; body= func_body body_decls body_stms }

let program (prog : _ Program.t) : Ast.Translation_unit.t =
  let globals = Program.globals prog in
  let functions = Program.functions prog in
  List.concat [decls globals; List.map ~f:(Tuple2.uncurry func) functions]
