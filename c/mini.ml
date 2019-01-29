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

include Ast_basic

type 'a named = (Identifier.t * 'a)
[@@deriving eq, sexp]

type 'a id_assoc = (Identifier.t, 'a) List.Assoc.t [@@deriving sexp]

module Type = struct
  type basic =
    | Int
    | Atomic_int
  [@@deriving sexp, variants, eq]
  ;;

  type t =
    | Normal of basic
    | Pointer_to of basic
  [@@deriving sexp, variants, eq]
  ;;

  let deref : t -> t Or_error.t = function
    | Pointer_to k -> Or_error.return (Normal k)
    | Normal _ -> Or_error.error_string "not a pointer type"
  ;;
end

module Initialiser = struct
  type t =
    { ty    : Type.t
    ; value : Constant.t option
    }
  [@@deriving sexp, make, eq]
  ;;

  module Named = struct
    type nonrec t = t named
    let equal : t -> t -> bool =
      Tuple2.equal ~eq1:Identifier.equal ~eq2:equal
    ;;
  end
end

module Lvalue = struct
  type t =
    | Variable of Identifier.t
    | Deref    of t
  [@@deriving sexp, variants, eq]

  let is_deref : t -> bool = function
    | Deref _ -> true
    | Variable _ -> false
  ;;

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
    | Eq          of t * t
    | Atomic_load of
        { src : Address.t
        ; mo  : Mem_order.t
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
            ~eq:(F.proc_variant2
                   (fun (l, r) ->
                      let open M.Let_syntax in
                      let%bind l' = map_m l ~f in
                      let%map  r' = map_m r ~f in
                      (l', r')))
            ~atomic_load:(
              fun v ~src ~mo ->
                let open M.Let_syntax in
                let%map src' = f src in
                v.constructor ~src:src' ~mo
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
            ~eq:(F.proc_variant2
                   (fun (l, r) ->
                      let open M.Let_syntax in
                      let%bind l' = map_m l ~f in
                      let%map  r' = map_m r ~f in
                      (l', r')))
            ~atomic_load:(
              fun v ~src ~mo ->
                let open M.Let_syntax in
                let%map src' = A.map_m ~f src in
                v.constructor ~src:src' ~mo
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
        { src : Expression.t
        ; dst : Address.t
        ; mo  : Mem_order.t
        }
    | Atomic_cmpxchg of
        { obj      : Address.t
        ; expected : Address.t
        ; desired  : Expression.t
        ; succ     : Mem_order.t
        ; fail     : Mem_order.t
        }
    | Nop
    | If_stm of { cond     : Expression.t
                ; t_branch : t
                ; f_branch : t option
                }
  [@@deriving sexp, variants]
  ;;

  (* Override to change f_branch into an optional argument. *)
  let if_stm
      ~(cond : Expression.t) ~(t_branch : t) ?(f_branch : t option) ()
    : t = if_stm ~cond ~t_branch ~f_branch
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap x ~assign ~atomic_store ~atomic_cmpxchg ~if_stm =
      let open M.Let_syntax in
      Variants.map x
        ~assign:(
          fun v ~lvalue ~rvalue ->
            let%map (lvalue', rvalue') = assign ~lvalue ~rvalue in
            v.constructor ~lvalue:lvalue' ~rvalue:rvalue'
        )
        ~atomic_store:(
          fun v ~src ~dst ~mo ->
            let%map (src', dst') = atomic_store ~src ~dst in
            v.constructor ~src:src' ~dst:dst' ~mo
        )
        ~atomic_cmpxchg:(
          fun v ~obj ~expected ~desired ~succ ~fail ->
            let%map (obj', expected', desired') =
              atomic_cmpxchg ~obj ~expected ~desired in
            v.constructor
              ~obj:obj' ~expected:expected' ~desired:desired'
              ~succ ~fail
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
          let open M.Let_syntax in
          B.bmap x
            ~assign:(
              fun ~lvalue ~rvalue -> both (f lvalue) (E.map_m ~f rvalue)
            )
            ~atomic_store:(
              fun ~src ~dst ->
                both (E.map_m ~f src) (A.map_m ~f dst)
            )
            ~atomic_cmpxchg:(
              fun ~obj ~expected ~desired ->
                let%bind obj'      = A.map_m ~f obj      in
                let%bind expected' = A.map_m ~f expected in
                let%map  desired'  = E.map_m ~f desired  in
                (obj', expected', desired')
            )
            ~if_stm:(
              fun ~cond ~t_branch ~f_branch ->
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
          let open M.Let_syntax in
          B.bmap x
            ~assign:(fun ~lvalue ~rvalue ->
                both (M.return lvalue) (E.map_m ~f rvalue))
            ~atomic_store:(
              fun ~src ~dst -> both (E.map_m ~f src) (f dst)
            )
            ~atomic_cmpxchg:(
              fun ~obj ~expected ~desired ->
                let%bind obj'      =          f obj      in
                let%bind expected' =          f expected in
                let%map  desired'  = E.map_m ~f desired  in
                (obj', expected', desired')
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
  [@@deriving sexp, fields, make]
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (func : t)
        ~(parameters : Type.t id_assoc -> Type.t id_assoc M.t)
        ~(body_decls : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(body_stms  : Statement.t list -> Statement.t list M.t)
      : t M.t =
      Fields.fold
        ~init:(M.return func)
        ~parameters:(F.proc_field parameters)
        ~body_decls:(F.proc_field body_decls)
        ~body_stms:(F.proc_field body_stms)
    ;;
  end

  let map = let module M = Base_map (Monad.Ident) in M.bmap

  module On_decls
    : Travesty.Traversable.S0_container with type t := t
                                         and type Elt.t = Initialiser.Named.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Initialiser.Named

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module L = Travesty.T_list.On_monad (M)

        let map_m
            (func : t)
            ~(f : Initialiser.Named.t -> Initialiser.Named.t M.t) =
          B.bmap func
            ~parameters:M.return
            ~body_decls:(L.map_m ~f)
            ~body_stms:M.return
      end
    end)
  ;;
end

module Program = struct
  type t =
    { globals   : Initialiser.t id_assoc
    ; functions : Function.t id_assoc
    }
  [@@deriving sexp, fields, make]
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (program : t)
        ~(globals   : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(functions : Function.t id_assoc -> Function.t id_assoc M.t)
      : t M.t =
      Fields.fold
        ~init:(M.return program)
        ~globals:(F.proc_field globals)
        ~functions:(F.proc_field functions)
    ;;
  end

  module On_decls
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t := Initialiser.Named.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Initialiser.Named

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module L = Travesty.T_list.On_monad (M)
        module F = Function.On_decls.On_monad (M)

        let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
          B.bmap program
            ~globals:(L.map_m ~f)
            ~functions:(L.map_m ~f:(fun (k, v) -> M.(F.map_m ~f v >>| Tuple2.create k)))
        ;;
      end
    end)
end

module Reify = struct
  let to_initialiser (value : Constant.t) : Ast.Initialiser.t =
    Assign (Constant value)
  ;;

  let basic_type_to_spec : Type.basic -> [> Ast.Type_spec.t] = function
    | Int -> `Int
    | Atomic_int -> `Defined_type (C_identifier.of_string "atomic_int")
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
    Identifier (C_identifier.of_string (Mem_order.to_string mo))
  ;;

  let known_call (name : string) (args : Ast.Expr.t list) : Ast.Expr.t =
    Call {func = Identifier (C_identifier.of_string name) ; arguments = args }
  ;;

  let rec expr : Expression.t -> Ast.Expr.t = function
    | Constant k -> Constant k
    | Lvalue l -> lvalue_to_expr l
    | Eq (l, r) -> Binary (expr l, `Eq, expr r)
    | Atomic_load { src; mo } ->
      known_call "atomic_load_explicit"
        [ address_to_expr src
        ; mem_order_to_expr mo
        ]
  ;;

  let known_call_stm (name : string) (args : Ast.Expr.t list) : Ast.Stm.t =
    Expr (Some (known_call name args))
  ;;

  let rec stm : Statement.t -> Ast.Stm.t = function
    | Assign { lvalue; rvalue } ->
      Expr (Some (Binary (lvalue_to_expr lvalue, `Assign, expr rvalue)))
    | Atomic_store { dst; src; mo } ->
      known_call_stm "atomic_store_explicit"
        [ address_to_expr dst
        ; expr src
        ; mem_order_to_expr mo
        ]
    | Atomic_cmpxchg { obj; expected; desired; succ; fail } ->
      known_call_stm "atomic_compare_exchange_strong_explicit"
        [ address_to_expr   obj
        ; address_to_expr   expected
        ; expr              desired
        ; mem_order_to_expr succ
        ; mem_order_to_expr fail
        ]
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
      let name (n, _) = Some (C_identifier.to_string n)
      let listing (_, fn) =
        List.map (Function.body_decls fn) ~f:(fun x -> `Decl x)
        @ List.map (Function.body_stms fn) ~f:(fun x -> `Stm x)
      let pp = Fmt.(using (Tuple2.uncurry Reify.func) Ast.External_decl.pp)
    end

    let name = "C"
  end)


module Litmus_ast = Litmus.Ast.Make (Litmus_lang)
module Litmus_pp = Litmus.Pp.Make_sequential (Litmus_ast)
