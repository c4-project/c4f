open Base
open Lib

module Source (Elt : T) = struct
  module Elt = Elt

  type t =
    | From_test of Fpath.t
    | Inline of Elt.t
end

module Post = struct
  type t = Sexp.t Litmus.Ast_base.Postcondition.t
end

module Post_source = Source (Post)

module Ext_source = Source (Sim_output.State.Set)

module Input = struct
  type t =
    | Postcondition of Post_source.t
    | Extensional of Ext_source.t
end

module Output = struct
  type t =
    | Disabled
    | Postcondition of Post.t
    | Full of Sim_output.t
end

module Make (R : Sim_runner.S) = struct
end

module Herd = Make (Lib.Herd)
