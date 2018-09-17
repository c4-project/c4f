open Rresult

(** [compiler id spec ps] runs the compiler with id [id] and spec [spec]
    using the paths in pathset [ps]. *)
val compile : string -> CompilerSpec.t -> Pathset.t -> (unit, R.msg) result

(** [test spec] checks whether the compiler with spec [spec] exists and is
    sensible. *)
val test : CompilerSpec.t -> (unit, R.msg) result
