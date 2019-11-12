(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: module signatures and basic types *)

open Base

(** {1 General signatures} *)

(** Signature of modules that expose a 'named' part of a mini-model element,
    usually for compatibility with functors. *)
module type S_named = sig
  type elt

  type t = Act_common.C_id.t * elt [@@deriving equal]
end

(** Signature of abstract data types that wrap some C variable name. *)
module type S_has_underlying_variable = sig
  type t
  (** The type that contains underlying variables. *)

  val variable_of : t -> Act_common.C_id.t
  (** [variable_of x] is the underlying variable of [x]. *)

  val variable_in_env : t -> env:_ Map.M(Act_common.C_id).t -> bool
  (** [variable_in_env x ~env] gets whether [x]'s underlying variable name is
      a key in an environment [env]. *)
end

(** Signature of parts of the mini-model that implement type checking. *)
module type S_type_checkable = sig
  type t
  (** The type being checked. *)

  module Type_check (E : Env_types.S) : sig
    val type_of : t -> Type.t Or_error.t
    (** [type_of x] tries to get the type of [x] given the variable typing
        environment [E.env]. It fails if the type is inconsistent. *)
  end
end

(** {2 Signatures for recursive modules} *)

module type S_address_traversable = sig
  type t

  type address

  (** Traversing over atomic-action addresses. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = address
end

module type S_lvalue_traversable = sig
  type t

  type lvalue

  (** Traversing over lvalues. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = lvalue
end

module type S_identifier_traversable = sig
  type t

  type identifier

  (** Traversing over identifiers. *)
  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t = identifier
end

(** Signature of c-mini modules that facilitate traversing over all of the
    typical features of a c-mini node, so long as any metadata has been
    fixed. *)
module type S_with_meta = sig
  type 'meta t

  type address

  type lvalue

  type identifier

  module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t
  (** We can traverse over the metadata. *)

  val erase_meta : 'meta t -> unit t
  (** [erase_meta x] deletes all of [x]'s metadata. *)

  (** By fixing the metadata type, we can perform various forms of standard
      traversal. *)
  module With_meta (Meta : T) : sig
    type nonrec t = Meta.t t

    include
      S_address_traversable with type t := t and type address := address

    include S_lvalue_traversable with type t := t and type lvalue := lvalue

    include
      S_identifier_traversable
        with type t := t
         and type identifier := identifier
  end
end

(** {1 Parametrised signatures of parts of c-mini}

    These two signatures exist because their implementing modules are
    mutually recursive.

    The implementations of these generally fix the type parameters at the top
    of the signature. *)

(** {2 Statements}

    Parametrised signature of statement implementations. *)
module type S_statement = sig
  type 'meta t [@@deriving sexp, equal]

  type 'meta assign
  (** Generally fixed to {!Assign.t}. *)

  type 'meta atomic_cmpxchg
  (** Generally fixed to {!Atomic_cmpxchg.t}. *)

  type 'meta atomic_store
  (** Generally fixed to {!Atomic_store.t}. *)

  type 'meta if_stm
  (** Generally fixed to {!Statement.If.t}. *)

  type 'meta while_loop
  (** Generally fixed to {!Statement.While.t}. *)

  (** {3 Constructors} *)

  val assign : 'meta assign -> 'meta t
  (** [assign a] lifts an assignment [a] to a statement. *)

  val atomic_cmpxchg : 'meta atomic_cmpxchg -> 'meta t
  (** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to a statement. *)

  val atomic_store : 'meta atomic_store -> 'meta t
  (** [atomic_store a] lifts an atomic store [a] to a statement. *)

  val if_stm : 'meta if_stm -> 'meta t
  (** [if_stm ifs] lifts an if statement [ifs] to a statement. *)

  val while_loop : 'meta while_loop -> 'meta t
  (** [while_loop loop] lifts a while or do-while loop [loop] to a statement. *)

  val nop : unit -> 'meta t
  (** [nop] is a no-operation statement; it corresponds to C's empty
      expression statement. *)

  (** {3 Accessors} *)

  val reduce :
       'meta t
    -> assign:('meta assign -> 'result)
    -> atomic_cmpxchg:('meta atomic_cmpxchg -> 'result)
    -> atomic_store:('meta atomic_store -> 'result)
    -> if_stm:('meta if_stm -> 'result)
    -> while_loop:('meta while_loop -> 'result)
    -> nop:(unit -> 'result)
    -> 'result
  (** [reduce stm ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop] applies
      the appropriate function of those given to [stm]. It does _not_
      recursively reduce statements inside blocks. *)

  val has_if_statements : 'meta t -> bool
  (** [has_if_statements stm] is true provided that [stm] is an if statement,
      or a composite statement for which [has_if_statements] is true for at
      least one sub-statement. *)

  val has_while_loops : 'meta t -> bool
  (** [has_while_loops stm] is true provided that [stm] is a while (or
      do-while) loop, or a composite statement for which [has_while_loops] is
      true for at least one sub-statement. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> assign:('m1 assign -> 'm2 assign M.t)
      -> atomic_cmpxchg:('m1 atomic_cmpxchg -> 'm2 atomic_cmpxchg M.t)
      -> atomic_store:('m1 atomic_store -> 'm2 atomic_store M.t)
      -> if_stm:('m1 if_stm -> 'm2 if_stm M.t)
      -> while_loop:('m1 while_loop -> 'm2 while_loop M.t)
      -> nop:(unit -> unit M.t)
      -> 'm2 t M.t
  end

  include S_with_meta with type 'meta t := 'meta t
end

(** {2 If statements}

    Parametrised signature of if-statement implementations. *)
module type S_if_statement = sig
  type 'meta expr
  (** Generally fixed to {!Expression.t}. *)

  type 'meta stm
  (** Generally fixed to {!Statement.t}. *)

  type 'meta t [@@deriving sexp, equal]
  (** Opaque type of if statements. *)

  (** {3 Constructors} *)

  val make :
       cond:'meta expr
    -> t_branch:('meta, 'meta stm) Block.t
    -> f_branch:('meta, 'meta stm) Block.t
    -> 'meta t
  (** [make ~cond t_branch f_branch] creates an if statement with condition
      [cond], true branch [t_branch], and false branch [f_branch]. *)

  (** {3 Accessors} *)

  val cond : 'meta t -> 'meta expr
  (** [cond ifs] gets [ifs]'s condition. *)

  val t_branch : 'meta t -> ('meta, 'meta stm) Block.t
  (** [t_branch ifs] gets [ifs]'s true branch. *)

  val f_branch : 'meta t -> ('meta, 'meta stm) Block.t
  (** [f_branch ifs] gets [ifs]'s false branch. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> cond:('m1 expr -> 'm2 expr M.t)
      -> t_branch:(('m1, 'm1 stm) Block.t -> ('m2, 'm2 stm) Block.t M.t)
      -> f_branch:(('m1, 'm1 stm) Block.t -> ('m2, 'm2 stm) Block.t M.t)
      -> 'm2 t M.t
  end

  include S_with_meta with type 'meta t := 'meta t
end

(** {2 While loops}

    Parametrised signature of while-loop implementations. *)
module type S_while_loop = sig
  type 'meta expr
  (** Generally fixed to {!Expression.t}. *)

  type 'meta stm
  (** Generally fixed to {!Statement.t}. *)

  type 'meta t [@@deriving sexp, equal]
  (** Opaque type of while loops. *)

  (** {3 Constructors} *)

  val make :
       cond:'meta expr
    -> body:('meta, 'meta stm) Block.t
    -> kind:[`Do_while | `While]
    -> 'meta t
  (** [make ~cond ~body ~kind] creates a while loop (with [~kind=`While]) or
      a do-while loop (with [~kind=`Do_while]) with condition [cond] and body
      [body]. *)

  (** {3 Accessors} *)

  val cond : 'meta t -> 'meta expr
  (** [cond loop] gets [loop]'s condition. *)

  val body : 'meta t -> ('meta, 'meta stm) Block.t
  (** [body loop] gets [loop]'s body. *)

  val kind : 'meta t -> [`While | `Do_while]
  (** [kind loop] gets whether [loop] is a while loop or a do-while loop. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> cond:('m1 expr -> 'm2 expr M.t)
      -> body:(('m1, 'm1 stm) Block.t -> ('m2, 'm2 stm) Block.t M.t)
      -> 'm2 t M.t
  end

  include S_with_meta with type 'meta t := 'meta t
end
