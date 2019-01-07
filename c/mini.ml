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

let map_combine
    (xs : 'a list) ~(f : 'a -> 'b Or_error.t) : 'b list Or_error.t =
  xs
  |> List.map ~f
  |> Or_error.combine_errors
;;

type 'a id_assoc = (Ast_basic.Identifier.t, 'a) List.Assoc.t

module Type = struct
  type t =
    | Int
    | Atomic_int
  [@@deriving sexp]
  ;;
end

module Initialiser = struct
  type t =
    { ty    : Type.t
    ; value : Ast_basic.Constant.t option
    }
  [@@deriving sexp]
  ;;
end

module Expression = struct
  type t =
    | Constant of Ast_basic.Constant.t
  [@@deriving sexp, variants]
  ;;
end

module Statement = struct
  type t =
    | Assign of { lvalue : Ast_basic.Identifier.t
                ; rvalue : Expression.t
                }
  [@@deriving sexp, variants]
  ;;
end

module Function = struct
  type t =
    { parameters : Type.t id_assoc
    ; body_decls : Initialiser.t id_assoc
    ; body_stms  : Statement.t list
    }
  ;;
end

module Program = struct
  type t =
    { globals   : Initialiser.t id_assoc
    ; functions : Function.t id_assoc
    }
  ;;
end

module Reify = struct
  let to_initialiser (value : Ast_basic.Constant.t) : Ast.Initialiser.t =
    Assign (Constant value)
  ;;

  let type_to_spec : Type.t -> [> Ast.Type_spec.t] = function
    | Int -> `Int
    | Atomic_int -> `Defined_type "atomic_int"
  ;;

  let id_declarator (id : Ast_basic.Identifier.t) : Ast.Declarator.t =
    { pointer = None; direct = Id id }
  ;;

  let decl (id : Ast_basic.Identifier.t) (elt : Initialiser.t) : Ast.Decl.t =
    { qualifiers = [ type_to_spec elt.ty ]
    ; declarator = [ { declarator  = id_declarator id
                     ; initialiser = Option.map ~f:to_initialiser elt.value
                     }
                   ]
    }

  let decls : Initialiser.t id_assoc -> [> `Decl of Ast.Decl.t ] list =
    List.map ~f:(fun (k, v) -> `Decl (decl k v))
  ;;

  let func_parameter
      (id : Ast_basic.Identifier.t)
      (ty : Type.t)
    : Ast.Param_decl.t =
    { qualifiers = [ type_to_spec ty ]
    ; declarator = `Concrete (id_declarator id)
    }

  let func_parameters
      (parameters : Type.t id_assoc) : Ast.Param_type_list.t =
    { params = List.map ~f:(Tuple2.uncurry func_parameter) parameters
    ; style  = `Normal
    }
  ;;

  let func_signature
      (id : Ast_basic.Identifier.t)
      (parameters : Type.t id_assoc)
    : Ast.Declarator.t =
    { pointer = None
    ; direct = Fun_decl (Id id, func_parameters parameters)
    }
  ;;

  let expr : Expression.t -> Ast.Expr.t = function
    | Constant k -> Constant k
  ;;

  let stm : Statement.t -> Ast.Stm.t = function
    | Assign { lvalue; rvalue } ->
      Expr (Some (Binary (Identifier lvalue, `Assign, expr rvalue)))
  ;;

  let func_body
      (ds : Initialiser.t id_assoc)
      (ss : Statement.t   list)
    : Ast.Compound_stm.t =
    decls ds @ List.map ~f:(fun x -> `Stm (stm x)) ss

  let func (id : Ast_basic.Identifier.t) (def : Function.t)
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

  let defined_types : (string, Type.t) List.Assoc.t =
    [ "atomic_int", Atomic_int ]

  let qualifiers_to_type (quals : [> Ast.Decl_spec.t ] list)
    : Type.t Or_error.t =
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
    | #Ast_basic.Type_qual.t as qual ->
      Or_error.error_s
        [%message "This type qualifier isn't supported (yet)"
            ~got:(qual : Ast_basic.Type_qual.t)]
    | #Ast_basic.Storage_class_spec.t as spec ->
      Or_error.error_s
        [%message "This storage-class specifier isn't supported (yet)"
            ~got:(spec : Ast_basic.Storage_class_spec.t)]
  ;;

  let name_of_declarator : Ast.Declarator.t ->
    Ast_basic.Identifier.t Or_error.t = function
    | { pointer = Some _; _ } ->
      Or_error.error_string "Pointers not supported yet"
    | { pointer = None;
        direct  = Id id } ->
      Or_error.return id
    | x ->
      Or_error.error_s
        [%message "Unsupported direct declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)
        ]
  ;;

  let value_of_initialiser
    : Ast.Initialiser.t -> Ast_basic.Constant.t Or_error.t = function
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
    : (Ast_basic.Identifier.t * Initialiser.t) Or_error.t =
    let open Or_error.Let_syntax in
    let%bind ty    = qualifiers_to_type d.qualifiers in
    let%bind idecl = Travesty.T_list.one d.declarator in
    let%bind name  = name_of_declarator idecl.declarator in
    let%map  value = Travesty.T_option.With_errors.map_m idecl.initialiser
        ~f:value_of_initialiser
    in
    (name, { Initialiser.ty; value })
  ;;

  let func (_f : Ast.Function_def.t)
    : (Ast_basic.Identifier.t * Function.t) Or_error.t =
    Or_error.unimplemented "soon"
  ;;

  let translation_unit (prog : Ast.Translation_unit.t) : Program.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind (ast_decls, ast_nondecls) = sift_decls prog in
    let%bind ast_funs = ensure_functions ast_nondecls in
    let%bind decls = map_combine ~f:decl ast_decls in
    let%map  funs = map_combine ~f:func ast_funs in
    { Program.globals = decls; functions = funs }
  ;;
end
