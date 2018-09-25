open Core

module type ContainerExtensions = sig
  type 'a cont

  val iter_result : ('a -> (unit, 'e) result) -> 'a cont -> (unit, 'e) result
  val max_measure : measure:('a -> int) -> ?default:int -> 'a cont -> int
end

module ContainerExtend (S : Container.S1)
       : (ContainerExtensions with type 'a cont = 'a S.t) = struct
  type 'a cont = 'a S.t

  let iter_result f = S.fold_result ~init:() ~f:(Fn.const f)

  let max_measure ~measure ?(default=0) xs =
    xs
    |> S.max_elt ~compare:(fun x y -> Int.compare (measure x) (measure y))
    |> Option.value_map ~f:measure ~default:default
end

module MyArray
       : (ContainerExtensions with type 'a cont = 'a array) =
  ContainerExtend(Array)

module MyList =
  struct
    include ContainerExtend(List)

    let exclude ~f xs = List.filter ~f:(Fn.non f) xs
  end

let%expect_test "MyList: max_measure on empty list" =
  printf "%d" (MyList.max_measure ~default:1066 ~measure:Fn.id []);
  [%expect {| 1066 |}]
