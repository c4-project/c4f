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

type 'a t = 'a option

include Traversable.Make_container1 (struct
    type 'a t = 'a option

    module On_monad (M : Monad.S) = struct
      let map_m xo ~f =
        let open M.Let_syntax in
        Option.fold xo
          ~init:(return None)
          ~f:(fun state x ->
              let%bind _ = state in
              let%map  x' = f x in
              Some x')
      ;;
    end
  end)
;;

let%expect_test "generated option map behaves properly: Some" =
  Format.printf "@[%a@]@."
    (My_format.pp_option ~pp:Int.pp)
    (map ~f:(fun x -> x * x) (Some 12));
  [%expect {| 144 |}]
;;

let%expect_test "generated option map behaves properly: None" =
  Format.printf "@[%a@]@."
    (My_format.pp_option ~pp:Int.pp)
    (map ~f:(fun x -> x * x) None);
  [%expect {| |}]
;;

let%expect_test "generated option count behaves properly: Some/yes" =
  Format.printf "@[%d@]@." (count ~f:Int.is_positive (Some 42));
  [%expect {| 1 |}]
;;

let%expect_test "generated option count behaves properly: Some/no" =
  Format.printf "@[%d@]@." (count ~f:Int.is_positive (Some (-42)));
  [%expect {| 0 |}]
;;

let%expect_test "mapM: returning identity on Some/Some" =
  let module M = On_monad (Option) in
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option
       ~pp:(My_format.pp_option ~pp:String.pp))
    (M.map_m ~f:(Option.some) (Some "hello"));
  [%expect {| hello |}]
;;

let first_some_of_thunks thunks =
  List.fold_until thunks
    ~init:()
    ~f:(fun () thunk ->
        Option.value_map (thunk ())
          ~default:(Container.Continue_or_stop.Continue ())
          ~f:(fun x -> Stop (Some x)))
    ~finish:(Fn.const None)
;;

let%expect_test "first_some_of_thunks: short-circuiting works" =
  Format.printf "@[<h>%a@]@."
    (My_format.pp_option ~pp:String.pp)
    (first_some_of_thunks
       [ Fn.const None
       ; Fn.const (Some "hello")
       ; Fn.const (Some "world")
       ; (fun () -> failwith "this shouldn't happen")
       ]);
  [%expect {| hello |}]
;;
