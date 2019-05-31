(* This file is part of 'act'.

Copyright (c) 2018, 2019 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

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

%token (* delimiters *) LBRACE RBRACE EOF EOL
%token (* main groups *) MACHINE COMPILER FUZZ
%token (* program subgroups *) CPP SIM
%token (* default resolutipn *) DEFAULT TRY
%token (* common keywords *) ENABLED CMD ARGV
%token (* fuzz-specific keywords *) ACTION WEIGHT
%token (* Herd-specific keywords *) ASM_MODEL C_MODEL
%token (* machine-specific keywords *) VIA SSH HOST USER COPY TO LOCAL
%token (* compiler-specific keywords *) STYLE ARCH

%token <bool>   BOOL
%token <string> STRING
%token <int>    INTEGER
%token <Act_common.Id.t>   IDENTIFIER

%type <Ast.t> main
%start main

%%

%inline braced(x):
  | xs = delimited(LBRACE, x, RBRACE) { xs }

%inline line_list(x):
  | xs = separated_nonempty_list(EOL, x?) { Base.List.filter_map ~f:Base.Fn.id xs }

%inline stanza(x):
  | xs = braced(line_list(x)) { xs }

simple_stanza(n, x):
  | xs = preceded(n, stanza(x)) { xs }

id_stanza(n, x):
  | n; id = IDENTIFIER; xs = stanza(x) { (id, xs) }

id_directive(n):
  | id = preceded(n, IDENTIFIER) { id }

main:
  | stanzas = line_list(top_stanza); EOF { stanzas }

top_stanza:
  | c = cpp_stanza      {                 Ast.Top.Cpp      c      }
  | d = default_stanza  {                 Ast.Top.Default  d      }
  | f = fuzz_stanza     {                 Ast.Top.Fuzz     f      }
  | x = machine_stanza  { let i, m = x in Ast.Top.Machine  (i, m) }

cpp_stanza:
  | items = simple_stanza(CPP, cpp_item) { items }

cpp_item:
  | b = enabled { Ast.Cpp.Enabled b  }
  | c = cmd     { Ast.Cpp.Cmd     c  }
  | vs = argv   { Ast.Cpp.Argv    vs }

default_stanza:
  | items = simple_stanza(DEFAULT, default_item) { items }

default_item:
  | TRY; cat = try_category; id = IDENTIFIER { Ast.Default.Try (cat, id) }

try_category:
  | ARCH     { Ast.Default.Category.Arch }
  | COMPILER { Ast.Default.Category.Compiler }
  | MACHINE  { Ast.Default.Category.Machine }
  | SIM      { Ast.Default.Category.Sim }

fuzz_stanza:
  | items = simple_stanza(FUZZ, fuzz_item) { items }

fuzz_item:
  | ACTION; action = IDENTIFIER; weight = fuzz_weight? { Ast.Fuzz.Action (action, weight) }

fuzz_weight:
  | WEIGHT; w = INTEGER { w }

sim_stanza:
  | s = id_stanza(SIM, sim_item) { s }

sim_item:
  | c = cmd                               { Ast.Sim.Cmd c }
  | s = id_directive(STYLE)               { Ast.Sim.Style s }
  | C_MODEL;   s = STRING                 { Ast.Sim.C_model s }
  | ASM_MODEL; e = IDENTIFIER; s = STRING { Ast.Sim.Asm_model (e, s) }

machine_stanza:
  | s = id_stanza(MACHINE, machine_item) { s }

machine_item:
  | b = enabled         {                 Ast.Machine.Enabled  b      }
  | VIA; v = via_stanza {                 Ast.Machine.Via      v      }
  | x = compiler_stanza { let i, c = x in Ast.Machine.Compiler (i, c) }
  | x = sim_stanza      { let i, s = x in Ast.Machine.Sim      (i, s) }

via_stanza:
  | LOCAL                                { Ast.Via.Local }
  | items = simple_stanza(SSH, ssh_item) { Ast.Via.Ssh items }

ssh_item:
  | USER;     user    = STRING { Ast.Ssh.User    user }
  | HOST;     host    = STRING { Ast.Ssh.Host    host }
  | COPY; TO; copy_to = STRING { Ast.Ssh.Copy_to copy_to }

compiler_stanza:
  | s = id_stanza(COMPILER, compiler_item) { s }

compiler_item:
  | b = enabled                    { Ast.Compiler.Enabled b }
  | c = cmd                        { Ast.Compiler.Cmd     c }
  | vs = argv                      { Ast.Compiler.Argv    vs }
  | style = id_directive(STYLE)    { Ast.Compiler.Style   style }
  | emits = id_directive(ARCH)     { Ast.Compiler.Emits   emits }

cmd:
  | CMD; c = STRING { c }

argv:
  | ARGV; vs = STRING+ { vs }

enabled:
  | ENABLED; b = BOOL { b }

