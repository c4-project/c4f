(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

include Traversable_intf

(** [Derived_ops_maker] is an internal module type used for
   implementing the derived operations (fold-map, fold, iterate) in an
   arity-generic way. *)
module type Derived_ops_maker = sig
  include Generic_types

  module On_monad :
    functor (M : Monad.S) ->
      (Generic_monadic with module M := M
                        and type 'a t := 'a t
                        and type 'a elt := 'a elt)
end

(** [Derived_ops_monadic_gen] is an internal functor used to generate
   several derived monadic operations (monadic fold-map, monadic
   iteration, etc) from a monadic traversal in an arity-generic
   way. *)
module Derived_ops_monadic_gen
    (I : Derived_ops_maker) (M : Monad.S) = struct
  (* We use the state monad to implement fold-map. *)
  module SM = State.Make2_transform (M)

  module IM = I.On_monad (M)

  let fold_mapM (type acc) c ~f ~init =
    let module SM' = State.To_S_transform (SM) (struct type t = acc end) in
    let module ISM = I.On_monad (SM') in
    SM.run' (ISM.mapM ~f:(fun x -> SM.Monadic.make (fun s -> f s x)) c) init
  ;;

  let foldM c ~init ~f =
    M.(
      fold_mapM ~init c ~f:(fun k x -> f k x >>| fun x' -> (x', x))
      >>| fst
    )
  ;;

  let iterM c ~f =
    M.(IM.mapM ~f:(fun x -> M.(f x >>| fun () -> x)) c >>| fun _ -> ())
  ;;

  let mapiM ~f c =
    M.(
      fold_mapM ~init:0 c
        ~f:(fun k x -> f k x >>| fun x' -> (k + 1, x'))
      >>| snd
    )
  ;;
end

(** Internal functor for generating several derived non-monadic,
   non-[Container] operations (map, iterate) from a fold-map, generic
   over both arity-0 and arity-1. *)
module Derived_ops_gen (I : Derived_ops_maker) = struct
  (* As usual, we just use the monadic equivalents over the identity
     monad. *)
  module D = Derived_ops_monadic_gen (I) (Monad.Ident)
  let fold_map = D.fold_mapM
  let mapi     = D.mapiM
end

(** [Container_gen] is an internal functor used to generate the input
   to [Container] functors in an arity-generic way. *)
module Container_gen (I : Derived_ops_maker) : sig
  val fold : 'a I.t -> init:'acc -> f:('acc -> 'a I.elt -> 'acc) -> 'acc
  val iter : [> `Custom of 'a I.t -> f:('a I.elt -> unit) -> unit ]
end = struct
  module D = Derived_ops_monadic_gen (I) (Monad.Ident)
  let fold = D.foldM
  let iter = `Custom D.iterM
end

module Make_container0 (I : Basic_container0)
  : Container0 with type t := I.t and type elt := I.Elt.t = struct
  (* [I] needs a bit of rearrangement to fit in the derived operation
     functors. *)
  module Maker = struct
    type 'a t = I.t
    type 'a elt = I.Elt.t
    module On_monad = I.On_monad
  end

  (* We can implement the non-monadic map using the identity monad. *)
  module Ident = I.On_monad (Monad.Ident)
  let map = Ident.mapM
  include Derived_ops_gen (Maker)

  include Container.Make0 (struct
      include I
      include Container_gen (Maker)
    end)

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (Maker) (MS)
  end

  module With_errors = On_monad (Core.Or_error)
end

module Make_container1 (I : Basic_container1)
  : Container1 with type 'a t := 'a I.t = struct
  (* [I] needs a bit of rearrangement to fit in the derived operation
     functors (as above, but slightly differently). *)
  module Maker = struct
    type 'a t = 'a I.t
    type 'a elt = 'a
    module On_monad = I.On_monad
  end

  module Ident = I.On_monad (Monad.Ident)
  let map = Ident.mapM
  include Derived_ops_gen (Maker)

  module C = Container.Make (struct
      type nonrec 'a t = 'a I.t
      include Container_gen (Maker)
    end)
  include C
  include My_container.Extend1 (struct
      type nonrec 'a t = 'a I.t
      include C
    end)

  let right_pad ~padding xs =
    let maxlen = max_measure ~measure:List.length xs
    and f = Fn.const padding
    in map ~f:(fun p -> p @ List.init (maxlen - List.length p) ~f) xs

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (Maker) (MS)

    (* [sequenceM] can't be defined on arity-0 containers. *)
    let sequenceM c = mapM ~f:(Fn.id) c
  end
  module With_errors = On_monad (Base.Or_error)

  module With_elt (Elt : Equal.S) =
    Make_container0 (struct
      type nonrec t = Elt.t I.t
      module Elt = Elt

      (* The [S0] fold-map has a strictly narrower function type than
         the [S1] one, so we can just supply the same [On_monad]. *)
      module On_monad (M : Monad.S) = On_monad (M)
    end)
end

module Helpers (M : Monad.S) = struct
  type 'a traversal = 'a -> 'a M.t

  let proc_variant0 f v =
    M.(f () >>| (fun () -> v.Base.Variant.constructor))
  ;;

  let proc_variant1 f v a =
    M.(f a >>| v.Base.Variant.constructor)
  ;;

  let proc_variant2 f v a b =
    let open M.Let_syntax in
    let%map (a', b') = f (a, b) in
    v.Base.Variant.constructor a' b'
  ;;

  let proc_variant3 f v a b c =
    let open M.Let_syntax in
    let%map (a', b', c') = f (a, b, c) in
    v.Base.Variant.constructor a' b' c'
  ;;

  let proc_field f state field =
    let open M.Let_syntax in
    let%bind container = state in
    let original = Field.get field container in
    let%map nval = f original in
    Field.fset field container nval
  ;;
end
