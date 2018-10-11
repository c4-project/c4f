open Core

module type Extensions = sig
  type 'a t

  val iter_result : ('a -> (unit, 'e) result) -> 'a t -> (unit, 'e) result
  val max_measure : measure:('a -> int) -> ?default:int -> 'a t -> int
end

module Extend (S : Container.S1) = struct
  type 'a t = 'a S.t

  let iter_result f = S.fold_result ~init:() ~f:(Fn.const f)

  let max_measure ~measure ?(default=0) xs =
    xs
    |> S.max_elt ~compare:(fun x y -> Int.compare (measure x) (measure y))
    |> Option.value_map ~f:measure ~default:default
end

module MyArray = Extend (Array)

module MyList = struct
  include Extend (List)
  include MyMonad.Extend (List)

  let exclude ~f xs = List.filter ~f:(Fn.non f) xs

  let%expect_test "MyList: max_measure on empty list" =
    printf "%d" (max_measure ~default:1066 ~measure:Fn.id []);
    [%expect {| 1066 |}]

  let%expect_test "MyList: exclude -ve numbers" =
    let excluded = exclude ~f:Int.is_negative
        [1; -1; 2; 10; -49; 0; 64]
    in
    Format.printf "@[%a@]@."
      (Format.pp_print_list ~pp_sep:MyFormat.pp_csep Int.pp) excluded;
    [%expect {| 1, 2, 10, 0, 64 |}]

  let right_pad ~padding xs =
    let maxlen = max_measure ~measure:List.length xs
    and f = Fn.const padding
    in
      List.map ~f:(fun p -> p @ List.init (maxlen - List.length p) ~f) xs

  let%expect_test "MyList: right_pad empty list" =
    Format.printf "@[%a@]@."
      (MyFormat.pp_listlist ~pp:Int.pp) (right_pad ~padding:2 []);
    [%expect {||}]

  let%expect_test "MyList: right_pad example list" =
    Format.printf "@[%a@]@."
      (MyFormat.pp_listlist ~pp:Int.pp)
      (right_pad ~padding:6
         [ [0; 8; 0; 0]
         ; [9; 9; 9]
         ; [8; 8; 1; 9; 9]
         ; [9; 1; 1; 9]
         ; [7; 2; 5]
         ; [3]
         ]);
    [%expect {|
                [ 0, 8, 0, 0, 6 ]
                [ 9, 9, 9, 6, 6 ]
                [ 8, 8, 1, 9, 9 ]
                [ 9, 1, 1, 9, 6 ]
                [ 7, 2, 5, 6, 6 ]
                [ 3, 6, 6, 6, 6 ] |}]

  let%expect_test "mapM: list" =
    Format.printf "@[<h>%a@]@."
      (MyFormat.pp_listlist ~pp:Int.pp)
      (mapM ~f:(fun k -> [k; 0])
         ([[1; 2; 3]]));
    [%expect {|
              [ 1, 2, 3 ]
              [ 1, 2, 0 ]
              [ 1, 0, 3 ]
              [ 1, 0, 0 ]
              [ 0, 2, 3 ]
              [ 0, 2, 0 ]
              [ 0, 0, 3 ]
              [ 0, 0, 0 ] |}]

  let prefixes xs =
    List.mapi ~f:(fun i _ -> List.take xs (i+1)) xs

  let%expect_test "prefixes: empty list" =
    Format.printf "@[<h>%a@]@."
      (MyFormat.pp_listlist ~pp:Int.pp)
      (prefixes []);
    [%expect {||}]

  let%expect_test "prefixes: sample list" =
    Format.printf "@[<h>%a@]@."
      (MyFormat.pp_listlist ~pp:Int.pp)
      (prefixes [1; 2; 3]);
    [%expect {|
              [ 1 ]
              [ 1, 2 ]
              [ 1, 2, 3 ] |}]
end
