(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common

module Pp_helpers = struct
  let pp_directive (pp_val : 'v Fmt.t) : (string * 'v) Fmt.t =
    Fmt.(hbox (pair ~sep:sp string pp_val))

  let pp_stanza_base (pp_header : 'h Fmt.t) (pp_directive : 'd Fmt.t) :
      ('h * 'd list) Fmt.t =
    Fmt.(
      vbox ~indent:2
        (pair ~sep:sp
           (hbox (pp_header ++ any "@ {"))
           (list ~sep:sp pp_directive))
      ++ any "@ }")

  let pp_simple_stanza (pp_directive : 'd Fmt.t) : (string * 'd list) Fmt.t =
    pp_stanza_base Fmt.string pp_directive

  let pp_id_stanza (pp_directive : 'd Fmt.t) :
      ((string * Id.t) * 'd list) Fmt.t =
    pp_stanza_base Fmt.(pair ~sep:sp string Id.pp) pp_directive

  let pp_qstring : string Fmt.t = Fmt.(quote string)

  let pp_bool : bool Fmt.t =
    Fmt.(using (fun b -> if b then "yes" else "no") string)

  let pp_id_directive : (string * Id.t) Fmt.t = pp_directive Id.pp

  let pp_cmd : string Fmt.t =
    Fmt.using (fun x -> ("cmd", x)) (pp_directive pp_qstring)

  let pp_argv : string list Fmt.t =
    Fmt.(
      using (fun x -> ("argv", x)) (pp_directive (list ~sep:sp pp_qstring)))

  let pp_enabled : bool Fmt.t =
    Fmt.using (fun x -> ("enabled", x)) (pp_directive pp_bool)
end

module Fuzz = struct
  type t = Action of Id.t * int option [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Action (id, Some c) ->
          pp_directive Fmt.(pair ~sep:sp Id.pp int) f ("action", (id, c))
      | Action (id, None) ->
          pp_id_directive f ("action", id))
end

module Backend = struct
  type t =
    | Cmd of string
    | Style of Id.t
    | C_model of string
    | Asm_model of Id.t * string
  [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Cmd c ->
          pp_cmd f c
      | Style s ->
          pp_id_directive f ("style", s)
      | C_model c ->
          pp_directive pp_qstring f ("c_model", c)
      | Asm_model (id, a) ->
          pp_directive
            Fmt.(pair ~sep:sp Id.pp pp_qstring)
            f
            ("asm_model", (id, a)))
end

module Ssh = struct
  type t = User of string | Host of string | Copy_to of string
  [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | User s ->
          pp_directive pp_qstring f ("user", s)
      | Host s ->
          pp_directive pp_qstring f ("host", s)
      | Copy_to s ->
          pp_directive pp_qstring f ("copy_to", s))

  let as_user : t -> string option = function
    | User u ->
        Some u
    | Host _ | Copy_to _ ->
        None

  let as_host : t -> string option = function
    | Host h ->
        Some h
    | User _ | Copy_to _ ->
        None

  let as_copy_to : t -> string option = function
    | Copy_to c ->
        Some c
    | Host _ | User _ ->
        None
end

module Via = struct
  type t = Local | Ssh of Ssh.t list [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Local ->
          Fmt.pf f "local"
      | Ssh s ->
          pp_simple_stanza Ssh.pp f ("ssh", s))
end

module Compiler = struct
  type t =
    | Enabled of bool [@sexp.bool]
    | Style of Id.t
    | Emits of Id.t
    | Cmd of string
    | Argv of string list [@sexp.list]
  [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Cmd c ->
          pp_cmd f c
      | Argv xs ->
          pp_argv f xs
      | Enabled b ->
          pp_enabled f b
      | Style s ->
          pp_id_directive f ("style", s)
      | Emits s ->
          pp_id_directive f ("arch", s))
end

module Machine = struct
  type t =
    | Compiler of Id.t * Compiler.t list [@sexp.list]
    | Enabled of bool
    | Via of Via.t
    | Backend of Id.t * Backend.t list [@sexp.list]
  [@@deriving sexp]

  let as_compiler : t -> (Id.t * Compiler.t list) option = function
    | Compiler (i, c) ->
        Some (i, c)
    | _ ->
        None

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Compiler (i, cs) ->
          pp_id_stanza Compiler.pp f (("compiler", i), cs)
      | Enabled b ->
          pp_enabled f b
      | Via v ->
          Fmt.(hbox (any "via@ " ++ Via.pp)) f v
      | Backend (i, ss) ->
          pp_id_stanza Backend.pp f (("backend", i), ss))
end

module Default = struct
  module Category = struct
    module M = struct
      type t = Arch | Compiler | Machine | Backend [@@deriving enum]

      let table : (t, string) List.Assoc.t =
        [ (Arch, "arch")
        ; (Compiler, "compiler")
        ; (Machine, "machine")
        ; (Backend, "backend") ]
    end

    include M
    include Act_utils.Enum.Extend_table (M)
  end

  type t = Try of Category.t * Id.t [@@deriving sexp]

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Try (c, i) ->
          pp_directive Fmt.(pair ~sep:sp Category.pp Id.pp) f ("try", (c, i)))
end

module Top = struct
  type t =
    | Default of Default.t list [@sexp.list]
    | Fuzz of Fuzz.t list [@sexp.list]
    | Machine of Id.t * Machine.t list [@sexp.list]
  [@@deriving sexp]

  let as_default : t -> Default.t list option = function
    | Default c ->
        Some c
    | _ ->
        None

  let as_fuzz : t -> Fuzz.t list option = function
    | Fuzz f ->
        Some f
    | _ ->
        None

  let as_machine : t -> (Id.t * Machine.t list) option = function
    | Machine (i, m) ->
        Some (i, m)
    | _ ->
        None

  let pp (f : Formatter.t) : t -> unit =
    Pp_helpers.(
      function
      | Machine (i, ms) ->
          pp_id_stanza Machine.pp f (("machine", i), ms)
      | Fuzz fs ->
          pp_simple_stanza Fuzz.pp f ("fuzz", fs)
      | Default ds ->
          pp_simple_stanza Default.pp f ("default", ds))
end

type t = (Top.t list[@sexp.list]) [@@deriving sexp]

let pp : t Fmt.t = Fmt.list ~sep:Fmt.(sp ++ cut) Top.pp
