(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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
open Utils

include Machine_intf

module Forward_basic_spec
    (I : Inherit.S)
    (B : Basic_spec with type t := I.c)
  : Basic_spec with type t := I.t and type via := B.via = struct
  module H = Inherit.Helpers (I)

  let runner        = H.forward B.runner
  let litmus        = H.forward B.litmus
  let ensure_litmus = H.forward B.ensure_litmus
  let via           = H.forward B.via
end

module Property = struct
  type t =
    | Id of Id.Property.t
    | Is_remote
    | Is_local
  [@@deriving sexp, variants]

  let eval (type r) ((module R) : (module Reference with type t = r))
      reference = function
    | Id prop   -> Id.Property.eval (R.id reference) prop
    | Is_remote -> R.remoteness reference = `Remote
    | Is_local  -> R.remoteness reference = `Local
  ;;

  let eval_b (type r) (rm : (module Reference with type t = r))
      reference expr =
    Blang.eval expr (eval rm reference)
  ;;

  let tree_docs : Property.Tree_doc.t =
    [ "id",
      { args = [ "PROPERTY" ]
      ; details =
          {| See 'identifier predicates'. |}
      }
    ; "is_remote",
      { args = []
      ; details =
          {| Selects machines that are known to be remote. |}
      }
    ; "is_local",
      { args = []
      ; details =
          {| Selects machines that are known to be local. |}
      }
    ]
  ;;

  let pp_tree : unit Fmt.t =
    Property.Tree_doc.pp tree_docs
      (List.map ~f:fst Variants.descriptions)
  ;;

  let%expect_test "all properties have documentation" =
    let num_passes =
      Variants.descriptions
      |> List.map ~f:fst
      |> List.map ~f:(List.Assoc.mem tree_docs ~equal:String.Caseless.equal)
      |> List.count ~f:not
    in
    Fmt.pr "@[<v>%d@]@." num_passes;
    [%expect {| 0 |}]
  ;;
end

module Ssh = struct
  type t =
    { host     : string
    ; user     : string sexp_option
    ; copy_dir : string
    } [@@deriving sexp, fields]
  ;;

  (* The use of 'sexp_option' above makes deriving this impossible. *)
  let create ~host ?user ~copy_dir = Fields.create ~host ~user ~copy_dir

  let pp f { host; user; copy_dir } =
    match user with
    | Some u -> Format.fprintf f "%s@@%s:%s" host u copy_dir
    | None   -> Format.fprintf f "%s:%s" host copy_dir
  ;;

  module To_config (C : sig val ssh : t end) : Ssh.S = struct
    let host = host C.ssh
    let user = user C.ssh
  end
end

module Id : sig
  include (module type of Id)
  include Reference with type t := t
end = struct
  include Id
  let id = Fn.id
  let default = of_string "default"
  let remoteness = Fn.const `Unknown
end

module Via = struct
  type t =
    | Local
    | Ssh of Ssh.t
  [@@deriving sexp, variants]
  ;;

  let pp f = function
    | Local -> String.pp f "local"
    | Ssh s -> Ssh.pp f s
  ;;

  let to_runner = function
    | Local -> (module Runner.Local : Runner.S)
    | Ssh c -> (
        module Utils.Ssh.Runner
            (struct
              include Ssh.To_config (struct let ssh = c end)
              let remote_dir = Fn.const c.copy_dir
            end)
          : Runner.S
      )
  ;;

  let remoteness = function
    | Local -> `Local
    (* Technically, if we're SSHing to loopback, this isn't true,
       but I suspect it doesn't matter. *)
    | Ssh _ -> `Remote
  ;;
end

module Spec = struct
  module M = struct
    type t =
      { enabled : bool [@default true] [@sexp_drop_default]
      ; via     : Via.t
      ; litmus  : Litmus_tool.Config.t sexp_option
      }
    [@@deriving sexp, fields, make]
    ;;

    let make ?enabled ?(via=Via.local) ?litmus () (* override *) =
      make ?enabled ~via ~litmus ()
    ;;

    (* We use a different name for the getter than the one
       [@@deriving fields] infers. *)
    let is_enabled = enabled

    let ensure_litmus (spec : t) : Litmus_tool.Config.t Or_error.t =
      spec
      |> litmus
      |> Result.of_option ~error:(
        Error.of_string
          "This machine doesn't have a valid litmus configuration."
      )
    ;;

    let pp_enabled (f : Base.Formatter.t) : bool -> unit = function
      | true  -> ()
      | false -> Fmt.unit "@ (DISABLED)" f ()
    ;;

    let pp = Fmt.(
        hbox (
          using (fun { via; enabled; _ } -> (via, enabled))
            (append Via.pp pp_enabled)
        )
      )
    ;;

    let pp_summary = pp (* for now *)

    let remoteness x = Via.remoteness (via x)
    let runner     x = Via.to_runner  (via x)
  end

  include M

  let default = { M.enabled = true; via = Local; litmus = None }

  module With_id = struct
    include Spec.With_id (M)

    include Forward_basic_spec (struct
        type nonrec t = t
        type c = M.t
        let component = spec
      end) (struct
        type via = Via.t
        include M
      end)
    ;;

    let is_enabled t = M.is_enabled (spec t)
    let remoteness t = M.remoteness (spec t)

    let default =
      create ~id:Id.default ~spec:default
    let pp f t =
      Format.fprintf f "@[%a@ (@,%a@,)@]"
        Id.pp (id   t)
        M.pp  (spec t)
    ;;
  end

  include Spec.Make (struct
      include M
      module With_id = With_id
    end)
end
