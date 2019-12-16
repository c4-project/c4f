(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** C-mini: module signatures for statements *)

open Base

(** {1 Parametrised statement signatures}

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

  val if_stm : 'meta if_stm -> 'meta t
  (** [if_stm ifs] lifts an if statement [ifs] to a statement. *)

  val while_loop : 'meta while_loop -> 'meta t
  (** [while_loop loop] lifts a while or do-while loop [loop] to a statement. *)

  val prim : 'meta Prim_statement.t -> 'meta t
  (** [prim p] lifts a primitive statement [p] to a statement. *)

  (** {4 Liftings of primitive statements} *)

  val assign : 'meta assign -> 'meta t
  (** [assign a] lifts an assignment [a] to a statement. *)

  val atomic_cmpxchg : 'meta atomic_cmpxchg -> 'meta t
  (** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to a statement. *)

  val atomic_store : 'meta atomic_store -> 'meta t
  (** [atomic_store a] lifts an atomic store [a] to a statement. *)

  val nop : 'meta -> 'meta t
  (** [nop meta] is a no-operation statement with metadata [meta]; it
      corresponds to C's empty expression statement. *)

  val label : 'meta Label.t -> 'meta t
  (** [label label] is a label statement with label [label]. *)

  val goto : 'meta Label.t -> 'meta t
  (** [goto label] is a GOTO statement with label [label]. *)

  val break : 'meta -> 'meta t
  (** [break meta] is a break statement with metadata [meta]. *)

  val continue : 'meta -> 'meta t
  (** [continue meta] is a continue statement with metadata [meta]. *)

  val return : 'meta -> 'meta t
  (** [return meta] is a return statement with metadata [meta]. *)

  val procedure_call : 'meta Call.t -> 'meta t
  (** [procedure_call a] lifts a procedure (non-value-returning function)
      call [a] to a statement. *)

  (** {3 Accessors} *)

  val reduce :
       'meta t
    -> prim:('meta Prim_statement.t -> 'result)
    -> if_stm:('meta if_stm -> 'result)
    -> while_loop:('meta while_loop -> 'result)
    -> 'result
  (** [reduce stm ~prim ~if_stm ~while_loop] applies the appropriate function
      of those given to [stm]. It does _not_ recursively reduce statements
      inside blocks. *)

  val is_if_statement : 'meta t -> bool
  (** [is_if_statement stm] is true provided that [stm] is an if statement. *)

  val has_if_statements : 'meta t -> bool
  (** [has_if_statements stm] is true provided that [stm] is an if statement,
      or a composite statement for which [has_if_statements] is true for at
      least one sub-statement. *)

  val has_while_loops : 'meta t -> bool
  (** [has_while_loops stm] is true provided that [stm] is a while (or
      do-while) loop, or a composite statement for which [has_while_loops] is
      true for at least one sub-statement. *)

  val has_blocks_with_metadata : 'meta t -> predicate:('meta -> bool) -> bool
  (** [has_blocks_with_metadata stm ~predicate] is true provided that [stm]
      has at least one block for which [predicate] is true on that block's
      metadata.

      This is useful for tracking things like the existence of dead-code. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> prim:('m1 Prim_statement.t -> 'm2 Prim_statement.t M.t)
      -> if_stm:('m1 if_stm -> 'm2 if_stm M.t)
      -> while_loop:('m1 while_loop -> 'm2 while_loop M.t)
      -> 'm2 t M.t
  end

  include Types.S_with_meta with type 'meta t := 'meta t
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

  include Types.S_with_meta with type 'meta t := 'meta t
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

  include Types.S_with_meta with type 'meta t := 'meta t
end
