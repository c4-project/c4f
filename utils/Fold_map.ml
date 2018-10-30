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

open Core

include Fold_map_intf

module Make (I : Basic)
  : S with type t = I.t and module Elt = I.Elt = struct
  type t = I.t
  module Elt = I.Elt

  (* We can implement the non-monadic fold-map using the identity
     monad. *)
  include
    ( I.On_monad (Monad.Ident)
      : Mappable with type t := t and type elt := Elt.t
    )

  module M = struct
    type nonrec t = t
    module Elt = I.Elt

    let fold c ~init ~f =
      fst (fold_map ~f:(fun acc x -> f acc x, x) ~init c)
    ;;

    let iter' c ~f =
      fst (fold_map ~f:(fun () x -> f x; (), x) ~init:() c)
    ;;

    let iter = `Custom iter'
  end
  include Container.Make0 (M)

  let map ~f c = snd (fold_map ~f:(fun () x -> (), f x) ~init:() c)

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)

    let mapM ~f c =
      MS.(
        fold_map ~f:(fun () x -> f x >>| Tuple2.create ()) ~init:() c
        >>| snd
      )
    ;;
  end

  module With_errors = On_monad (Base.Or_error)
end

(*
 * Implementations for common containers
 *)

module List (Elt : Equal.S)
  : S with type t := Elt.t list
       and module Elt := Elt =
  Make (struct
    type t = Elt.t list
    module Elt = Elt

    module On_monad (M : Monad.S) = struct
      let fold_map ~f ~init xs =
        let open M.Let_syntax in
        let%map (acc_final, xs_final) =
          List.fold_left xs
            ~init:(return (init, []))
            ~f:(fun state x ->
                let%bind (acc, xs') = state in
                let%map  (acc', x') = f acc x in
                (acc', x'::xs'))
        in
        (acc_final, List.rev xs_final)
      ;;
    end
  end)

let%expect_test "generated list map behaves properly" =
  let module IntList = List (Int) in
  Format.printf "@[%a@]@."
    (Format.pp_print_list Int.pp ~pp_sep:Format.pp_print_space)
    (IntList.map ~f:(fun x -> x * x) [ 1; 3; 5; 7 ]);
  [%expect {| 1 9 25 49 |}]
;;

let%expect_test "generated list count behaves properly" =
  let module IntList = List (Int) in
  Format.printf "@[%d@]@." (IntList.count ~f:Int.is_positive [ -7; -5; -3; -1; 1; 3; 5; 7 ]);
  [%expect {| 4 |}]
;;

module Option (Elt : Equal.S)
  : S with type t := Elt.t option
       and module Elt := Elt =
  Make (struct
    type t = Elt.t option
    module Elt = Elt

    module On_monad (M : Monad.S) = struct
      let fold_map ~f ~init xo =
        let open M.Let_syntax in
        Option.fold xo
          ~init:(return (init, None))
          ~f:(fun state x ->
              let%bind (acc , _ ) = state in
              let%map  (acc', x') = f acc x in
              (acc', Some x'))
      ;;
    end
  end)
