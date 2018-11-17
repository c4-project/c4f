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

include Fold_map_intf

(** Internal functor for generating several derived monadic operations
    (monadic fold, monadic map, monadic iteration) from a monadic
    fold-map, generic over both arity-0 and arity-1. *)
module Derived_ops_monadic_gen (I: Generic_monadic) = struct
  let foldM c ~init ~f =
    I.M.(
      I.fold_mapM ~init c ~f:(fun k x -> f k x >>| fun x' -> (x', x))
      >>| fst
    )
  ;;

  let mapM ~f c =
    I.M.(
      I.fold_mapM ~f:(fun () x -> f x >>| Tuple2.create ()) ~init:() c
      >>| snd
    )
  ;;

  let iterM c ~f = foldM ~init:() ~f:(Fn.const f) c

  let mapiM ~f c =
    I.M.(
      I.fold_mapM ~init:0 c
        ~f:(fun k x -> f k x >>| fun x' -> (k + 1, x'))
      >>| snd
    )
  ;;
end

(** Internal functor for generating several derived non-monadic,
   non-[Container] operations (map, iterate) from a fold-map, generic
   over both arity-0 and arity-1. *)
module Derived_ops_gen (I: Generic) = struct
  (* As usual, we just use the monadic equivalents over the identity
     monad. *)
  module D = Derived_ops_monadic_gen (struct
      module M = Monad.Ident
      include I
      let fold_mapM = I.fold_map
    end)
  ;;
  let iter = D.iterM
  let map  = D.mapM
  let mapi = D.mapiM
end

(** Inner module used to generate the input to [Container] functors
    from non-monadic fold-maps. *)
module Container_gen (I : Generic) : sig
  val fold : 'a I.t -> init:'acc -> f:('acc -> 'a I.elt -> 'acc) -> 'acc
  val iter : [> `Custom of 'a I.t -> f:('a I.elt -> unit) -> unit ]
end = struct
  module D = Derived_ops_monadic_gen (struct
      module M = Monad.Ident
      include I
      let fold_mapM = I.fold_map
    end)
  ;;
  let fold = D.foldM
  let iter = `Custom D.iterM
end

module Make_container0 (I : Basic_container0)
  : Container0 with type t := I.t and type elt := I.Elt.t = struct
  (* We can implement the non-monadic fold-map using the identity
     monad. *)
  include I.On_monad (Monad.Ident)
  let fold_map = fold_mapM

  module Gen
    : Generic with type 'a t = I.t and type 'a elt = I.Elt.t = struct
    type 'a t = I.t
    type 'a elt = I.Elt.t
    include I.On_monad (Monad.Ident)
    let fold_map = fold_mapM
  end

  include Container.Make0 (struct
      type t = I.t
      module Elt = I.Elt
      include Container_gen (Gen)
    end)

  include Derived_ops_gen (Gen)

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (struct
        type 'a t = I.t
        type 'a elt = I.Elt.t
        module M = MS
        include I.On_monad (MS)
      end)
  end
  module With_errors = On_monad (Core.Or_error)
end

module Make_container1 (I : Basic_container1)
  : Container1 with type 'a t := 'a I.t = struct
  include I.On_monad (Monad.Ident)
  let fold_map = fold_mapM

  module Gen
    : Generic with type 'a t = 'a I.t and type 'a elt = 'a = struct
    type 'a t = 'a I.t
    type 'a elt = 'a
    include I.On_monad (Monad.Ident)
    let fold_map = fold_mapM
  end

  module C = Container.Make (struct
      type nonrec 'a t = 'a I.t
      include Container_gen (Gen)
    end)
  include C
  include My_container.Extend1 (struct
      type nonrec 'a t = 'a I. t
      include C
    end)
  include Derived_ops_gen (Gen)

  let right_pad ~padding xs =
    let maxlen = max_measure ~measure:List.length xs
    and f = Fn.const padding
    in map ~f:(fun p -> p @ List.init (maxlen - List.length p) ~f) xs

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)
    include Derived_ops_monadic_gen (struct
        type 'a t = 'a I.t
        type 'a elt = 'a
        module M = MS
        include I.On_monad (MS)
      end)
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
  let proc_variant0 f init v =
    M.(f init () >>| (fun (s, ()) -> (s, v.Base.Variant.constructor)))
  ;;

  let proc_variant1 f init v a =
    M.(f init a >>| Tuple2.map_snd ~f:v.Base.Variant.constructor)
  ;;

  let proc_variant2 f init v a b =
    let open M.Let_syntax in
    let%map (init, (a', b')) = f init (a, b) in
    (init, v.Base.Variant.constructor a' b')
  ;;

  let proc_variant3 f init v a b c =
    let open M.Let_syntax in
    let%map (init, (a', b', c')) = f init (a, b, c) in
    (init, v.Base.Variant.constructor a' b' c')
  ;;

  let proc_field f state field _container original =
    let open M.Let_syntax in
    let%bind (acc,  container) = state in
    let%map  (acc', nval) = f acc original in
    (acc', Field.fset field container nval)
  ;;

  let fold_nop acc v = M.return (acc, v)
end

let chain mapper ~f init = mapper ~f ~init
