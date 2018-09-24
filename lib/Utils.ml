open Core

module type ContainerExtensions = sig
  type 'a cont

  val iter_result : ('a -> (unit, 'e) result) -> 'a cont -> (unit, 'e) result
end

module ContainerExtend (S : Container.S1)
       : (ContainerExtensions with type 'a cont = 'a S.t) = struct
  type 'a cont = 'a S.t

  let iter_result f = S.fold_result ~init:() ~f:(fun _ -> f)
end

module Array
       : (ContainerExtensions with type 'a cont = 'a array) =
  ContainerExtend(Array)

module List : (ContainerExtensions with type 'a cont = 'a list) =
  ContainerExtend(List)

let has_extension ~ext file =
  snd (Filename.split_extension file) = Some ext
