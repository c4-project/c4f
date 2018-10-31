open Core

(* NB: Unlike the versions in Fold_map, these are
   second-order types and support changing the element
   type on mapping.  This might need unifying later on. *)

module type Extensions = sig
  type 'a t

  val mapM
    :  f:('a -> 'b t)
    -> ('a list) t
    -> ('b list) t

  val mapiM
    :  f:(int -> 'a -> 'b t)
    -> ('a list) t
    -> ('b list) t

  val tapM
    :  f:('a -> unit t)
    -> 'a
    -> 'a t

  val tap
    :  f:('a -> unit)
    -> 'a
    -> 'a t
end

module Extend (M : Monad.S) = struct
  type 'a t = 'a M.t

  let mapiM ~f xsc =
    let open M.Let_syntax in
    let%bind xs = xsc in
    let%bind (i', xsc') =
      List.fold_left
        ~init:(return (0, []))
        ~f:(fun mc x ->
            let%bind (i, m') = mc in
            let%bind x' = f i x in
            (return (i + 1, x'::m')))
        xs
    in
    assert (i' = List.length xs);
    return (List.rev xsc')

  let mapM ~f xsc = mapiM ~f:(Fn.const f) xsc

  let tapM ~f a = M.(f a >>| Fn.const a)

  let tap ~f a = f a; M.return a
end

module MyOption = Extend (Option)
module MyOr_error = Extend (Or_error)

let%expect_test "mapiM: returning identity on option" =
  Format.printf "@[<h>%a@]@."
    (MyFormat.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:MyFormat.pp_csep String.pp))
    (MyOption.mapiM ~f:(fun _ k -> Some k)
       (Some ["a"; "b"; "c"; "d"; "e"]));
  [%expect {| a, b, c, d, e |}]

let%expect_test "mapiM: counting upwards on option" =
  Format.printf "@[<h>%a@]@."
    (MyFormat.pp_option
       ~pp:(Format.pp_print_list ~pp_sep:MyFormat.pp_csep Int.pp))
    (MyOption.mapiM ~f:(fun i _ -> Some i)
       (Some ["a"; "b"; "c"; "d"; "e"]));
  [%expect {| 0, 1, 2, 3, 4 |}]

let%expect_test "tapM on option, where the tap returns None" =
  Format.printf "@[%a@]@."
    (MyFormat.pp_option ~pp:Int.pp)
    (MyOption.tapM ~f:(Fn.const None) 10);
  [%expect {||}]

let%expect_test "tapM on option, where the tap returns Some" =
  Format.printf "@[%a@]@."
    (MyFormat.pp_option ~pp:Int.pp)
    (MyOption.tapM ~f:(Fn.const (Some ())) 10);
  [%expect {| 10 |}]

