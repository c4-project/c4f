(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* delimiters *)
%token LBRACE "{"
%token RBRACE "}"
%token COLON  ":"
%token EOF EOL

%token (* main groups *) MACHINE COMPILER BACKEND FUZZ
%token (* default resolutipn *) DEFAULT TRY
%token (* common keywords *) ENABLED CMD ARGV
%token (* fuzz-specific keywords *) ACTION WEIGHT SET PARAM FLAG RATIO
%token (* Herd-specific keywords *) ASM_MODEL C_MODEL
%token (* machine-specific keywords *) VIA SSH HOST USER COPY LOCAL
%token (* compiler-specific keywords *) STYLE ARCH
%token (* noise words *) TO

%token <bool>   BOOL
%token <string> STRING
%token <int>    INTEGER
%token <Act_common.Id.t>   IDENTIFIER

%type <Ast.t> main
%start main

%%

let braced(x) == delimited(LBRACE, x, RBRACE)

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
  | ~ = default_stanza ; <                 Ast.Top.Default         >
  | ~ = fuzz_stanza    ; <                 Ast.Top.Fuzz            >
  | x = machine_stanza ; { let i, m = x in Ast.Top.Machine  (i, m) }

let default_stanza := simple_stanza(DEFAULT, default_item)

let default_item :=
  | TRY; ~ = try_category; ~ = IDENTIFIER; < Ast.Default.Try >

let try_category :=
  | ARCH     ; { Ast.Default.Category.Arch }
  | COMPILER ; { Ast.Default.Category.Compiler }
  | MACHINE  ; { Ast.Default.Category.Machine }
  | BACKEND  ; { Ast.Default.Category.Backend }

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

let backend_stanza := id_stanza(BACKEND, backend_item)

let backend_item :=
  | ~ = cmd                               ; < Ast.Backend.Cmd       >
  | ~ = argv                              ; < Ast.Backend.Argv      >
  | ~ = id_directive(STYLE)               ; < Ast.Backend.Style     >
  | C_MODEL;   ~ = STRING                 ; < Ast.Backend.C_model   >
  | ASM_MODEL; ~ = IDENTIFIER; s = STRING ; < Ast.Backend.Asm_model >

let machine_stanza := id_stanza(MACHINE, machine_item)

let machine_item :=
  | ~ = enabled         ; <                 Ast.Machine.Enabled         >
  | VIA; ~ = via_stanza ; <                 Ast.Machine.Via             >
  | x = compiler_stanza ; { let i, c = x in Ast.Machine.Compiler (i, c) }
  | x = backend_stanza  ; { let i, s = x in Ast.Machine.Backend  (i, s) }

let via_stanza :=
  | LOCAL                           ; { Ast.Via.Local }
  | ~ = simple_stanza(SSH, ssh_item); < Ast.Via.Ssh   >

let ssh_item :=
  | USER;      ~ = STRING ; < Ast.Ssh.User    >
  | HOST;      ~ = STRING ; < Ast.Ssh.Host    >
  | COPY; TO?; ~ = STRING ; < Ast.Ssh.Copy_to >

let compiler_stanza := id_stanza(COMPILER, compiler_item)

let compiler_item :=
  | ~ = enabled             ; < Ast.Compiler.Enabled >
  | ~ = cmd                 ; < Ast.Compiler.Cmd     >
  | ~ = argv                ; < Ast.Compiler.Argv    >
  | ~ = id_directive(STYLE) ; < Ast.Compiler.Style   >
  | ~ = id_directive(ARCH)  ; < Ast.Compiler.Emits   >

let cmd := CMD; STRING

let argv := ARGV; STRING+

let enabled := ENABLED; BOOL

