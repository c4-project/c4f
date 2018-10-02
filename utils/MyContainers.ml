open Core

module type ContainerExtensions =
sig
  type 'a cont

  val iter_result : ('a -> (unit, 'e) result) -> 'a cont -> (unit, 'e) result
  val max_measure : measure:('a -> int) -> ?default:int -> 'a cont -> int
end

module ContainerExtend (S : Container.S1) =
struct
  type 'a cont = 'a S.t

  let iter_result f = S.fold_result ~init:() ~f:(Fn.const f)

  let max_measure ~measure ?(default=0) xs =
    xs
    |> S.max_elt ~compare:(fun x y -> Int.compare (measure x) (measure y))
    |> Option.value_map ~f:measure ~default:default
end

module MyArray = ContainerExtend(Array)

module MyList =
struct
  include ContainerExtend(List)

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

  let pp_listlist =
    Format.printf "@[<v>%a@]@."
    (Format.pp_print_list
         ~pp_sep:Format.pp_print_cut
         (fun f k ->
           Format.fprintf f "@[<h>%a@]"
           (Format.pp_print_list
              ~pp_sep:MyFormat.pp_csep Int.pp)
           k))

  let%expect_test "MyList: right_pad empty list" =
    pp_listlist (right_pad ~padding:2 []);
    [%expect {||}]

  let%expect_test "MyList: right_pad example list" =
    pp_listlist
      (right_pad ~padding:6
         [ [0; 8; 0; 0]
         ; [9; 9; 9]
         ; [8; 8; 1; 9; 9]
         ; [9; 1; 1; 9]
         ; [7; 2; 5]
         ; [3]
         ]);
    [%expect {|
                0, 8, 0, 0, 6
                9, 9, 9, 6, 6
                8, 8, 1, 9, 9
                9, 1, 1, 9, 6
                7, 2, 5, 6, 6
                3, 6, 6, 6, 6 |}]
end
