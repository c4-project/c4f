open Core_kernel

let null_formatter () = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())
let pp_listlist ~pp = Fmt.(vbox (list ~sep:cut (hvbox (brackets (list ~sep:comma pp)))))

let pp_c_braces (pi : 'a Fmt.t) : 'a Fmt.t =
  Fmt.(hvbox (suffix (unit "@ }") (hvbox ~indent:4 (prefix (unit "{@ ") pi))))
;;

let%expect_test "pp_c_braces: short items" =
  Fmt.(
    pr
      "@[%a@]@."
      (pp_c_braces (fun f () ->
           string f "eggs";
           sp f ();
           string f "ham"))
      ());
  [%expect {| { eggs ham } |}]
;;

let%expect_test "pp_c_braces: things before and after" =
  Fmt.(
    pr
      "@[poached@ %a@ sandwich@]@."
      (pp_c_braces (fun f () ->
           string f "eggs";
           sp f ();
           string f "ham"))
      ());
  [%expect {| poached { eggs ham } sandwich |}]
;;

let%expect_test "pp_c_braces: long items" =
  Fmt.(
    pr
      "@[%a@]@."
      (pp_c_braces (fun f () ->
           string f "a very long string that'll doubtless wrap the box";
           sp f ();
           string f "ham and cheese and cheese and ham"))
      ());
  [%expect
    {|
    {
        a very long string that'll doubtless wrap the box
        ham and cheese and cheese and ham
    } |}]
;;

let%expect_test "pp_c_braces: long items; things before and after" =
  Fmt.(
    pr
      "@[this is@ %a@ and cheese@]@."
      (pp_c_braces (fun f () ->
           string f "a very long string that'll doubtless wrap the box";
           sp f ();
           string f "ham and cheese and cheese and ham"))
      ());
  [%expect
    {|
    this is
    {
        a very long string that'll doubtless wrap the box
        ham and cheese and cheese and ham
    } and cheese |}]
;;

let pp_kv f k pv v =
  Fmt.(hvbox ~indent:1 (pair ~sep:(const (suffix sp char) ':') string pv)) f (k, v)
;;
