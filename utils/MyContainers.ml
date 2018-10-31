open Core

type 'a partial_order =
  [ `Equal
  | `Subset of 'a
  | `Superset of 'a
  | `NoOrder
  ] [@@deriving sexp]
;;

module type SetExtensions = sig
  type t

  val disjoint : t -> t -> bool
  val partial_compare : t -> t -> t partial_order
end

module SetExtend (S : Set.S) : SetExtensions with type t := S.t = struct
  let disjoint x y = S.(is_empty (inter x y));;

  (** [drop_left x p] updates partial order [p] with the information that
      an element [x] exists in the left hand set that isn't in the right
      hand set. *)
  let drop_left x = function
    | `Equal -> `Superset (S.singleton x)
    | `Superset xs -> `Superset (S.add xs x)
    | `NoOrder | `Subset _ -> `NoOrder
  ;;

  (** [drop_right x p] updates partial order [p] with the information
     that an element [x] exists in the right hand set that isn't in
     the left hand set. *)
  let drop_right x = function
    | `Equal -> `Subset (S.singleton x)
    | `Subset xs -> `Subset (S.add xs x)
    | `Superset _ | `NoOrder -> `NoOrder
  ;;

  let partial_compare x y =
    Sequence.fold (S.symmetric_diff x y)
      ~init:`Equal
      ~f:(fun po ->
          function
          | First  x -> drop_left  x po
          | Second x -> drop_right x po
        )
  ;;
end

let%expect_test "disjoint: positive witness" =
  let module M = SetExtend (Int.Set) in
  printf "%b"
    (M.disjoint
       (Int.Set.of_list [2; 4; 6; 8])
       (Int.Set.of_list [3; 5; 7; 9]));
  [%expect {| true |}]

let%expect_test "disjoint: negative witness" =
  let module M = SetExtend (Int.Set) in
  printf "%b"
    (M.disjoint
       (Int.Set.of_list [2; 4; 6; 8])
       (Int.Set.of_list [1; 2; 3; 4]));
  [%expect {| false |}]

let%expect_test "disjoint: double empty is disjoint" =
  let module M = SetExtend (Int.Set) in
  printf "%b" (M.disjoint (Int.Set.empty) (Int.Set.empty));
  [%expect {| true |}]

let%expect_test "partial_compare: empty sets" =
  let module M = SetExtend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( M.partial_compare (Int.Set.empty) (Int.Set.empty)
             : Int.Set.t partial_order
           )];
  [%expect {| Equal |}]

let%expect_test "partial_compare: subset" =
  let module M = SetExtend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3 ] [ 1; 2; 3; 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| (Subset (4 5 6)) |}]

let%expect_test "partial_compare: superset" =
  let module M = SetExtend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4; 5; 6 ] [ 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| (Superset (1 2 3)) |}]

let%expect_test "partial_compare: equal" =
  let module M = SetExtend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4; 5; 6 ] [ 6; 5; 4; 3; 2; 1 ]
             : Int.Set.t partial_order
           )];
  [%expect {| Equal |}]

let%expect_test "partial_compare: no order" =
  let module M = SetExtend (Int.Set) in
  Sexp.output_hum Out_channel.stdout
    [%sexp ( MyFn.on Int.Set.of_list M.partial_compare
               [ 1; 2; 3; 4 ] [ 3; 4; 5; 6 ]
             : Int.Set.t partial_order
           )];
  [%expect {| NoOrder |}]


module MyList = struct
  include Fold_map.List
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
