open Core

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
end

module MyOption = Extend (Option)

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

