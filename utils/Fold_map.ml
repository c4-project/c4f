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

module Make0 (I : Basic0)
  : S0 with type t = I.t and type elt = I.Elt.t = struct
  type t = I.t
  type elt = I.Elt.t

  (* We can implement the non-monadic fold-map using the identity
     monad. *)
  include
    ( I.On_monad (Monad.Ident)
      : Mappable0 with type t := t and type elt := elt
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

  let max_measure ~measure ?(default=0) xs =
    xs
    |> max_elt ~compare:(MyFn.on measure Int.compare)
    |> Option.value_map ~f:measure ~default:default

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)

    let mapM ~f c =
      MS.(
        fold_map ~f:(fun () x -> f x >>| Tuple2.create ()) ~init:() c
        >>| snd
      )
    ;;

    let mapiM ~f c =
      MS.(
        fold_map ~init:0 c
          ~f:(
            fun k x ->
              let open MS.Let_syntax in
              let%map x' = f k x in (k + 1, x')
          )
        >>| snd
      )
    ;;
  end

  module With_errors = On_monad (Base.Or_error)
end

module Make1 (I : Basic1)
  : S1 with type 'a t = 'a I.t = struct
  type 'a t = 'a I.t

  (* We can implement the non-monadic fold-map using the identity
     monad. *)
  include
    ( I.On_monad (Monad.Ident)
      : Mappable1 with type 'a t := 'a t
    )

  module M = struct
    type nonrec 'a t = 'a t

    let fold c ~init ~f =
      fst (fold_map ~f:(fun acc x -> f acc x, x) ~init c)
    ;;

    let iter' c ~f =
      fst (fold_map ~f:(fun () x -> f x; (), x) ~init:() c)
    ;;

    let iter = `Custom iter'
  end
  include Container.Make (M)

  let map ~f c = snd (fold_map ~f:(fun () x -> (), f x) ~init:() c)

  let max_measure ~measure ?(default=0) xs =
    xs
    |> max_elt ~compare:(MyFn.on measure Int.compare)
    |> Option.value_map ~f:measure ~default:default

  let right_pad ~padding xs =
    let maxlen = max_measure ~measure:List.length xs
    and f = Fn.const padding
    in map ~f:(fun p -> p @ List.init (maxlen - List.length p) ~f) xs

  module On_monad (MS : Monad.S) = struct
    include I.On_monad (MS)

    let mapM ~f c =
      MS.(
        fold_map ~f:(fun () x -> f x >>| Tuple2.create ()) ~init:() c
        >>| snd
      )
    ;;

    let mapiM ~f c =
      MS.(
        fold_map ~init:0 c
          ~f:(
            fun k x ->
              let open MS.Let_syntax in
              let%map x' = f k x in (k + 1, x')
          )
        >>| snd
      )
    ;;
  end

  module With_errors = On_monad (Base.Or_error)

  module To_S0 (Elt : Equal.S) =
    Make0 (struct
      type nonrec t = Elt.t t
      module Elt = Elt

      (* The [S0] fold-map has a strictly narrower function type than
         the [S1] one, so we can just supply the same [On_monad]. *)
      module On_monad (M : Monad.S) = On_monad (M)
    end)
end

(*
 * Implementations for common containers
 *)

module List : S1 with type 'a t = 'a list =
  Make1 (struct
    type 'a t = 'a list

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
  Format.printf "@[%a@]@."
    (Format.pp_print_list Int.pp ~pp_sep:Format.pp_print_space)
    (List.map ~f:(fun x -> x * x) [ 1; 3; 5; 7 ]);
  [%expect {| 1 9 25 49 |}]
;;

let%expect_test "generated list count behaves properly" =
  Format.printf "@[%d@]@." (List.count ~f:Int.is_positive [ -7; -5; -3; -1; 1; 3; 5; 7 ]);
  [%expect {| 4 |}]
;;

let%expect_test "mapiM: returning identity on list/option" =
  let module M = List.On_monad (Option) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:My_format.pp_csep String.pp))
    (M.mapiM ~f:(fun _ k -> Some k) ["a"; "b"; "c"; "d"; "e"]);
  [%expect {| a, b, c, d, e |}]

let%expect_test "mapiM: counting upwards on list/option" =
  let module M = List.On_monad (Option) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:My_format.pp_csep Int.pp))
    (M.mapiM ~f:(fun i _ -> Some i) [3; 7; 2; 4; 42]);
  [%expect {| 0, 1, 2, 3, 4 |}]

module Option : S1 with type 'a t = 'a option =
  Make1 (struct
    type 'a t = 'a option

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

module Singleton : S1 with type 'a t = 'a =
  Make1 (struct
    type 'a t = 'a

    module On_monad (M : Monad.S) = struct
      let fold_map ~f ~init x = f init x
    end
  end)

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
