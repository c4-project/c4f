(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* delimiters *)
%token LBRACE "{"
%token RBRACE "}"
%token COLON  ":"
%token EOF EOL

%token (* main groups *) FUZZ
%token (* fuzz-specific keywords *) ACTION WEIGHT SET PARAM FLAG RATIO
%token (* noise words *) TO

%token <bool>   BOOL
%token <int>    INTEGER
%token <C4f_common.Id.t>   IDENTIFIER

%start <Ast.t> main

%%

let braced(x) == delimited("{", x, "}")

let line_list(x) ==
  | ~ = separated_nonempty_list(EOL, x?); < Base.List.filter_opt >

let stanza(x) == braced(line_list(x))

let simple_stanza(n, x) := preceded(n, stanza(x))

let id_stanza(n, x) :=
  | n; ~ = IDENTIFIER; ~ = stanza(x); <>

let id_directive(n) := preceded(n, IDENTIFIER)

let main :=
  | ~ = line_list(top_stanza); EOF; <>

let top_stanza :=
  | ~ = fuzz_stanza    ; <Ast.Top.Fuzz>

let fuzz_stanza := simple_stanza(FUZZ, fuzz_item)

let fuzz_flag_value :=
  | RATIO; ~ = INTEGER; COLON; ~ = INTEGER ; < Ast.Fuzz.Flag_value.Ratio >
  | ~ = BOOL                               ; < Ast.Fuzz.Flag_value.Exact >

let fuzz_setter :=
  | PARAM; ~ = IDENTIFIER; TO?; ~ = INTEGER        ; < Ast.Fuzz.Setter.Param >
  | FLAG; ~ = IDENTIFIER; TO?; ~ = fuzz_flag_value ; < Ast.Fuzz.Setter.Flag  >

let fuzz_item :=
  | ACTION; ~ = IDENTIFIER; ~ = fuzz_weight?; < Ast.Fuzz.Action >
  | SET; ~ = fuzz_setter                    ; < Ast.Fuzz.Set    >

let fuzz_weight := WEIGHT; INTEGER