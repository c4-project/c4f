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

include Ast_basic

let map_combine
    (xs : 'a list) ~(f : 'a -> 'b Or_error.t) : 'b list Or_error.t =
  xs
  |> List.map ~f
  |> Or_error.combine_errors
;;

type 'a named = (Identifier.t * 'a)
[@@deriving sexp]

type 'a id_assoc = (Identifier.t, 'a) List.Assoc.t
[@@deriving sexp]

module Mem_order = struct
  module M = struct
    type t =
      | Seq_cst
      | Release
      | Acquire
      | Rel_acq
      | Relaxed
      | Consume
    [@@deriving enum]
    ;;

    let table =
      [ Seq_cst, "memory_order_seq_cst"
      ; Release, "memory_order_release"
      ; Acquire, "memory_order_acquire"
      ; Rel_acq, "memory_order_rel_acq"
      ; Relaxed, "memory_order_relaxed"
      ; Consume, "memory_order_consume"
      ]
    ;;
  end
  include M
  include Utils.Enum.Extend_table (M)
end

module Type = struct
  type basic =
    | Int
    | Atomic_int
  [@@deriving sexp]
  ;;

  type t =
    | Normal of basic
    | Pointer_to of basic
  [@@deriving sexp]
  ;;

  let int : t = Normal Int
  let atomic_int : t = Normal Atomic_int
end

module Initialiser = struct
  type t =
    { ty    : Type.t
    ; value : Constant.t option
    }
  [@@deriving sexp, make]
  ;;
end

module Lvalue = struct
  type t =
    | Variable of Identifier.t
    | Deref    of t
  [@@deriving sexp, variants, eq]

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Identifier.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Identifier

      module On_monad (M : Monad.S) = struct
        module F = Travesty.Traversable.Helpers (M)

        let rec map_m x ~f =
          Variants.map x
            ~variable:(F.proc_variant1 f)
            ~deref:(F.proc_variant1 (map_m ~f))
      end
  end)

  let rec underlying_variable : t -> Identifier.t = function
    | Variable id -> id
    | Deref    t  -> underlying_variable t
  ;;
end

module Address = struct
  type t =
    | Lvalue of Lvalue.t
    | Ref    of t
  [@@deriving sexp, variants, eq]

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module F = Travesty.Traversable.Helpers (M)

        let rec map_m x ~f =
          Variants.map x
            ~lvalue:(F.proc_variant1 f)
            ~ref:(F.proc_variant1 (map_m ~f))
      end
  end)

  let rec underlying_variable : t -> Identifier.t = function
    | Lvalue lv -> Lvalue.underlying_variable lv
    | Ref    t  -> underlying_variable t
  ;;
end

module Expression = struct
  type t =
    | Constant    of Constant.t
    | Lvalue      of Lvalue.t
    | Equals      of t * t
    | Atomic_load of
        { src   : Address.t
        ; order : Mem_order.t
        }
  [@@deriving sexp, variants]
  ;;

  module On_addresses
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module F = Travesty.Traversable.Helpers (M)

        let rec map_m x ~f =
          Variants.map x
            ~constant:(F.proc_variant1 M.return)
            ~lvalue:(F.proc_variant1 M.return)
            ~equals:(F.proc_variant2
                       (fun (l, r) ->
                          let open M.Let_syntax in
                          let%bind l' = map_m l ~f in
                          let%map  r' = map_m r ~f in
                          (l', r')))
            ~atomic_load:(
              fun v ~src ~order ->
                let open M.Let_syntax in
                let%map src' = f src in
                v.constructor ~src:src' ~order
            )
      end
  end)

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module A = Address.On_lvalues.On_monad (M)
        module F = Travesty.Traversable.Helpers (M)

        let rec map_m x ~f =
          Variants.map x
            ~constant:(F.proc_variant1 M.return)
            ~lvalue:(F.proc_variant1 f)
            ~equals:(F.proc_variant2
                       (fun (l, r) ->
                          let open M.Let_syntax in
                          let%bind l' = map_m l ~f in
                          let%map  r' = map_m r ~f in
                          (l', r')))
            ~atomic_load:(
              fun v ~src ~order ->
                let open M.Let_syntax in
                let%map src' = A.map_m ~f src in
                v.constructor ~src:src' ~order
            )
      end
  end)

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_lvalues
      end)
      (Lvalue.On_identifiers)
end

module Statement = struct
  type t =
    | Assign of { lvalue : Lvalue.t
                ; rvalue : Expression.t
                }
    | Atomic_store of
        { src   : Expression.t
        ; dst   : Address.t
        ; order : Mem_order.t
        }
    | Nop
    | If_stm of { cond     : Expression.t
                ; t_branch : t
                ; f_branch : t option
                }
  [@@deriving sexp, variants]
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap x ~assign ~atomic_store ~if_stm =
      let open M.Let_syntax in
      Variants.map x
        ~assign:(
          fun v ~lvalue ~rvalue ->
            let%map (lvalue', rvalue') = assign ~lvalue ~rvalue in
            v.constructor ~lvalue:lvalue' ~rvalue:rvalue'
        )
        ~atomic_store:(
          fun v ~src ~dst ~order ->
            let%map (src', dst') = atomic_store ~src ~dst in
            v.constructor ~src:src' ~dst:dst' ~order
        )
        ~if_stm:(
          fun v ~cond ~t_branch ~f_branch ->
            let%map (cond', t_branch', f_branch') =
              if_stm ~cond ~t_branch ~f_branch in
            v.constructor
              ~cond:cond' ~t_branch:t_branch' ~f_branch:f_branch'
        )
        ~nop:(F.proc_variant0 M.return)
    ;;
  end

  module On_lvalues
    : Travesty.Traversable.S0_container
        with type t := t
         and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module A = Address.On_lvalues.On_monad (M)
        module E = Expression.On_lvalues.On_monad (M)
        module O = Travesty.T_option.On_monad (M)

        let both (x : 'a M.t) (y : 'b M.t) : ('a * 'b) M.t =
          let open M.Let_syntax in
          let%bind x' = x in
          let%map  y' = y in
          (x', y')
        ;;

        let rec map_m x ~f =
          B.bmap x
            ~assign:(
              fun ~lvalue ~rvalue -> both (f lvalue) (E.map_m ~f rvalue)
            )
            ~atomic_store:(
              fun ~src ~dst ->
                both (E.map_m ~f src) (A.map_m ~f dst)
            )
            ~if_stm:(
              fun ~cond ~t_branch ~f_branch ->
                let open M.Let_syntax in
                let%bind cond'     = E.map_m ~f cond in
                let%bind t_branch' = map_m t_branch ~f in
                let%map  f_branch' = O.map_m f_branch ~f:(map_m ~f) in
                (cond', t_branch', f_branch')
            )
      end
  end)

  module On_addresses
    : Travesty.Traversable.S0_container
        with type t := t
         and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)
        module O = Travesty.T_option.On_monad (M)

        let both (x : 'a M.t) (y : 'b M.t) : ('a * 'b) M.t =
          let open M.Let_syntax in
          let%bind x' = x in
          let%map  y' = y in
          (x', y')
        ;;

        let rec map_m x ~f =
          B.bmap x
            ~assign:(fun ~lvalue ~rvalue ->
                both (M.return lvalue) (E.map_m ~f rvalue))
            ~atomic_store:(
              fun ~src ~dst -> both (E.map_m ~f src) (f dst)
            )
            ~if_stm:(
              fun ~cond ~t_branch ~f_branch ->
                let open M.Let_syntax in
                let%bind cond'     = E.map_m ~f cond in
                let%bind t_branch' = map_m t_branch ~f in
                let%map  f_branch' = O.map_m f_branch ~f:(map_m ~f) in
                (cond', t_branch', f_branch')
            )
      end
  end)

  module On_identifiers
    : Travesty.Traversable.S0_container
        with type t := t
         and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_lvalues
      end)
      (Lvalue.On_identifiers)
end

module Function = struct
  type t =
    { parameters : Type.t id_assoc
    ; body_decls : Initialiser.t id_assoc
    ; body_stms  : Statement.t list
    }
  [@@deriving sexp, fields]
  ;;

  let map (func : t)
      ~(parameters : (Type.t id_assoc -> Type.t id_assoc))
      ~(body_decls : (Initialiser.t id_assoc -> Initialiser.t id_assoc))
      ~(body_stms  : (Statement.t list -> Statement.t list))
    : t =
    Fields.Direct.map func
      ~parameters:(fun _ _ -> parameters)
      ~body_decls:(fun _ _ -> body_decls)
      ~body_stms:(fun _ _ -> body_stms)
end

module Program = struct
  type t =
    { globals   : Initialiser.t id_assoc
    ; functions : Function.t id_assoc
    }
  [@@deriving sexp, fields, make]
  ;;
end

module Reify = struct
  let to_initialiser (value : Constant.t) : Ast.Initialiser.t =
    Assign (Constant value)
  ;;

  let basic_type_to_spec : Type.basic -> [> Ast.Type_spec.t] = function
    | Int -> `Int
    | Atomic_int -> `Defined_type "atomic_int"
  ;;

  let type_to_spec : Type.t -> [> Ast.Type_spec.t] = function
    | Normal     x
    | Pointer_to x -> basic_type_to_spec x
  ;;

  let type_to_pointer : Type.t -> Pointer.t option = function
    | Normal     _ -> None
    | Pointer_to _ -> Some [[]]
  ;;

  let id_declarator
      (ty : Type.t) (id : Identifier.t)
    : Ast.Declarator.t =
    { pointer = type_to_pointer ty; direct = Id id }
  ;;

  let decl (id : Identifier.t) (elt : Initialiser.t) : Ast.Decl.t =
    { qualifiers = [ type_to_spec elt.ty ]
    ; declarator = [ { declarator  = id_declarator elt.ty id
                     ; initialiser = Option.map ~f:to_initialiser elt.value
                     }
                   ]
    }

  let decls : Initialiser.t id_assoc -> [> `Decl of Ast.Decl.t ] list =
    List.map ~f:(fun (k, v) -> `Decl (decl k v))
  ;;

  let func_parameter
      (id : Identifier.t)
      (ty : Type.t)
    : Ast.Param_decl.t =
    { qualifiers = [ type_to_spec ty ]
    ; declarator = `Concrete (id_declarator ty id)
    }

  let func_parameters
      (parameters : Type.t id_assoc) : Ast.Param_type_list.t =
    { params = List.map ~f:(Tuple2.uncurry func_parameter) parameters
    ; style  = `Normal
    }
  ;;

  let func_signature
      (id : Identifier.t)
      (parameters : Type.t id_assoc)
    : Ast.Declarator.t =
    { pointer = None
    ; direct = Fun_decl (Id id, func_parameters parameters)
    }
  ;;

  let rec lvalue_to_expr : Lvalue.t -> Ast.Expr.t = function
    | Variable v -> Identifier v
    | Deref    l -> Prefix (`Deref, lvalue_to_expr l)
  ;;

  let rec address_to_expr : Address.t -> Ast.Expr.t = function
    | Lvalue v -> lvalue_to_expr v
    | Ref    l -> Prefix (`Ref, address_to_expr l)
  ;;

  let mem_order_to_expr (mo : Mem_order.t) : Ast.Expr.t =
    Identifier (Mem_order.to_string mo)
  ;;

  let rec expr : Expression.t -> Ast.Expr.t = function
    | Constant k -> Constant k
    | Lvalue l -> lvalue_to_expr l
    | Equals (l, r) -> Binary (expr l, `Eq, expr r)
    | Atomic_load { src; order } ->
      Call { func      = Identifier "atomic_load_explicit"
           ; arguments = [ address_to_expr src
                         ; mem_order_to_expr order
                         ]
           }
  ;;

  let rec stm : Statement.t -> Ast.Stm.t = function
    | Assign { lvalue; rvalue } ->
      Expr (Some (Binary (lvalue_to_expr lvalue, `Assign, expr rvalue)))
    | Atomic_store { dst; src; order } ->
      Expr
        (Some
           (Call
              { func      = Identifier "atomic_store_explicit"
              ; arguments = [ address_to_expr dst
                            ; expr src
                            ; mem_order_to_expr order
                            ]
              }
           )
        )
    | If_stm { cond; t_branch; f_branch } ->
      If { cond = expr cond
         ; t_branch = stm t_branch
         ; f_branch = Option.map ~f:stm f_branch
         }
    | Nop -> Expr None
  ;;

  let func_body
      (ds : Initialiser.t id_assoc)
      (ss : Statement.t   list)
    : Ast.Compound_stm.t =
    decls ds @ List.map ~f:(fun x -> `Stm (stm x)) ss

  let func (id : Identifier.t) (def : Function.t)
    : Ast.External_decl.t =
    `Fun
      { decl_specs = [ `Void ]
      ; signature  = func_signature id def.parameters
      ; decls      = []
      ; body       = func_body def.body_decls def.body_stms
      }
  ;;

  let program (prog : Program.t) : Ast.Translation_unit.t =
    List.concat
      [ decls                             prog.globals
      ; List.map ~f:(Tuple2.uncurry func) prog.functions
      ]
  ;;
end

module Litmus_lang : Litmus.Ast.Basic
  with type Statement.t = [`Stm of Statement.t | `Decl of Initialiser.t named]
   and type Program.t = Function.t named
   and type Constant.t = Constant.t = (struct
    module Constant = Constant

    module Statement = struct
      type t = [`Stm of Statement.t | `Decl of Initialiser.t named]
      [@@deriving sexp]
      let pp =
        Fmt.using
          (function
            | `Decl (id, init) -> `Decl (Reify.decl id init)
            | `Stm stm         -> `Stm  (Reify.stm stm))
          Ast.Litmus_lang.Statement.pp

      let empty () = `Stm (Statement.nop)
      let make_uniform = Travesty.T_list.right_pad ~padding:(empty ())
    end

    module Program = struct
      type t = Function.t named [@@deriving sexp]
      let name (n, _) = Some n
      let listing (_, fn) =
        List.map (Function.body_decls fn) ~f:(fun x -> `Decl x)
        @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)
      let pp = Fmt.(using (Tuple2.uncurry Reify.func) Ast.External_decl.pp)
    end

    let name = "C"
  end)


module Litmus_ast = struct
  module A = Litmus.Ast.Make (Litmus_lang)
  include A
  include Litmus.Pp.Make_sequential (A)
end

module Convert = struct
  (** [sift_decls maybe_decl_list] tries to separate [maybe_decl_list]
     into a list of declarations followed immediately by a list of
     code, C89-style. *)
  let sift_decls :
    ([> `Decl of Ast.Decl.t ] as 'a) list -> (Ast.Decl.t list * ('a list)) Or_error.t =
    Travesty.T_list.With_errors.fold_m
      ~init:([], [])
      ~f:(fun (decls, rest) item ->
          match decls, rest, item with
          | _, [], `Decl d -> Or_error.return (d::decls, rest)
          | _, _ , `Decl _ -> Or_error.error_string
                                "Declarations must go before code."
          | _, _ , _       -> Or_error.return (decls, item::rest)
        )
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

  let defined_types : (string, Type.basic) List.Assoc.t =
    [ "atomic_int", Atomic_int ]

  let qualifiers_to_basic_type (quals : [> Ast.Decl_spec.t ] list)
    : Type.basic Or_error.t =
    let open Or_error.Let_syntax in
    match%bind Travesty.T_list.one quals with
    | `Int -> return Type.Int
    | `Defined_type t ->
      t
      |> List.Assoc.find ~equal:String.equal defined_types
      |> Result.of_option
        ~error:(Error.create_s
                  [%message "Unknown defined type" ~got:t])
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

  let make_type (basic_type : Type.basic) (is_pointer : bool) : Type.t =
    if is_pointer then Pointer_to basic_type else Normal basic_type
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
    let ty = make_type basic_type is_pointer in
    (name, { Initialiser.ty; value })
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
      let ty = make_type basic_type is_pointer in (id, ty)
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
    match%bind expr_to_lvalue expr with
    | Variable var -> return var
    | Deref _ as lv ->
      Or_error.error_s
        [%message "Expected identifier" ~got:(lv : Lvalue.t)]
  ;;

  let expr_to_memory_order (expr : Ast.Expr.t) : Mem_order.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind id = expr_to_identifier expr in
    id
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
      (call_table : (Ast.Expr.t list -> 'a Or_error.t) id_assoc)
      (func : Ast.Expr.t)
      (arguments : Ast.Expr.t list)
    : 'a Or_error.t =
    let open Or_error.Let_syntax in
      let%bind func_name = expr_to_identifier func in
      let%bind call_handler =
        func_name
        |> List.Assoc.find ~equal:String.equal call_table
        |> Result.of_option
          ~error:(
            Error.create_s
              [%message "Unsupported function in expression position"
                  ~got:func_name
              ]
          )
      in call_handler arguments
  ;;

  let rec expr
    : Ast.Expr.t -> Expression.t Or_error.t =
    let open Or_error.Let_syntax in
    let model_binary l op r =
      let%bind l' = expr l
      and      r' = expr r
      in
      match op with
      | `Eq -> return (Expression.equals l' r')
      | _   -> Or_error.error_s
                 [%message "Unsupported binary operator" ~got:(op : Operators.Bin.t)]
    in
    let model_atomic_load_explicit = function
      | [ raw_src; raw_mo ] ->
        let%map src   = expr_to_address raw_src
        and     order = expr_to_memory_order raw_mo
        in Expression.atomic_load ~src ~order
      | args ->
        Or_error.error_s
          [%message "Invalid arguments to atomic_load_explicit"
              ~got:(args : Ast.Expr.t list)
          ]
    in
    let call_table =
      [ "atomic_load_explicit", model_atomic_load_explicit ]
    in
    let model_call = call call_table
    in
    function
    | Brackets e -> expr e
    | Binary (l, op, r) -> model_binary l op r
    | Constant k -> Or_error.return (Expression.constant k)
    | Identifier id ->
      Or_error.return (Expression.lvalue (Lvalue.variable id))
    | Prefix (`Deref, expr) ->
      Or_error.(expr |> expr_to_lvalue >>| Lvalue.deref >>| Expression.lvalue)
    | Call { func; arguments } -> model_call func arguments
    | Prefix _ | Postfix _ | Ternary _ | Cast _
    | Subscript _ | Field _ | Sizeof_type _ | String _
      as e ->
      Or_error.error_s
        [%message "Unsupported expression" ~got:(e : Ast.Expr.t)]
  ;;

  let expr_stm : Ast.Expr.t -> Statement.t Or_error.t =
    let open Or_error.Let_syntax in
    let model_atomic_store_explicit = function
      | [ raw_dst; raw_src; raw_mo ] ->
        let%map dst   = expr_to_address raw_dst
        and     src   = expr raw_src
        and     order = expr_to_memory_order raw_mo
        in Statement.atomic_store ~dst ~src ~order
      | args ->
        Or_error.error_s
          [%message "Invalid arguments to atomic_store_explicit"
              ~got:(args : Ast.Expr.t list)
          ]
    in
    let call_table =
      [ "atomic_store_explicit", model_atomic_store_explicit ]
    in
    let model_call = call call_table
    in
    function
    | Binary (l, `Assign, r) ->
      let%map lvalue = expr_to_lvalue l
      and     rvalue = expr r
      in Statement.assign ~lvalue ~rvalue
    | Call { func; arguments } -> model_call func arguments
    | Brackets _ | Constant _
    | Prefix _ | Postfix _ | Binary _ | Ternary _ | Cast _
    | Subscript _ | Field _ | Sizeof_type _ | String _ | Identifier _
      as e ->
      Or_error.error_s
        [%message "Unsupported expression statement" ~got:(e : Ast.Expr.t)]
  ;;

  let rec stm : Ast.Stm.t -> Statement.t Or_error.t =
    let model_if old_cond old_t_branch old_f_branch =
      let open Or_error.Let_syntax in
      let%map cond = expr old_cond
      and     t_branch = stm old_t_branch
      and     f_branch =
        Travesty.T_option.With_errors.map_m old_f_branch ~f:stm
      in Statement.If_stm { cond; t_branch; f_branch }
    in
    function
    | Expr None -> Or_error.return Statement.nop
    | Expr (Some e) -> expr_stm e
    | If { cond; t_branch; f_branch } -> model_if cond t_branch f_branch
    | Continue | Break | Return _ | Label _ | Compound _
    | Switch _ | While _ | Do_while _ | For _ | Goto _
        as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]
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
    in (name, { Function.parameters; body_decls; body_stms })
  ;;

  let translation_unit (prog : Ast.Translation_unit.t) : Program.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind (ast_decls, ast_nondecls) = sift_decls prog in
    let%bind ast_funs = ensure_functions ast_nondecls in
    let%map  decls = map_combine ~f:decl ast_decls
    and      funs  = map_combine ~f:func ast_funs
    in { Program.globals = decls; functions = funs }
  ;;

  module Litmus_conv = Litmus.Ast.Convert (struct
      module From = struct
        include Ast.Litmus
        module Lang = Ast.Litmus_lang
      end
      module To = Litmus_ast

      let program = func
      let constant = Or_error.return
    end)
  ;;

  let litmus
    : Ast.Litmus.Validated.t
      -> Litmus_ast.Validated.t Or_error.t =
    Litmus_conv.convert
  ;;
end
