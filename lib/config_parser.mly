(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

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
%token (* main groups *) MACHINE COMPILER
%token (* program subgroups *) CPP HERD
%token (* common keywords *) ENABLED CMD ARGV DEFAULT
%token (* Herd-specific keywords *) ASM_MODEL C_MODEL
%token (* machine-specific keywords *) VIA SSH HOST USER COPY TO LOCAL
%token (* compiler-specific keywords *) STYLE EMITS

%token <bool>   BOOL
%token <string> STRING
%token <Id.t> IDENTIFIER

%type <Config_ast.t> main
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

id_stanza(n, i, x):
  | n; id = i; xs = stanza(x) { (id, xs) }


main:
  | stanzas = line_list(top_stanza); EOF { stanzas }

top_stanza:
  | c = cpp_stanza      {                 Config_ast.Top.Cpp      c      }
  | h = herd_stanza     {                 Config_ast.Top.Herd     h      }
  | x = machine_stanza  { let i, m = x in Config_ast.Top.Machine  (i, m) }
  | x = compiler_stanza { let i, c = x in Config_ast.Top.Compiler (i, c) }

cpp_stanza:
  | items = simple_stanza(CPP, cpp_item) { items }

cpp_item:
  | b = enabled { Config_ast.Cpp.Enabled b  }
  | c = cmd     { Config_ast.Cpp.Cmd     c  }
  | vs = argv   { Config_ast.Cpp.Argv    vs }

herd_stanza:
  | items = simple_stanza(HERD, herd_item) { items }

herd_item:
  | c = cmd                               { Config_ast.Herd.Cmd c }
  | C_MODEL;   s = STRING                 { Config_ast.Herd.C_model s }
  | ASM_MODEL; e = IDENTIFIER; s = STRING { Config_ast.Herd.Asm_model (e, s) }

id_or_default:
  | id = IDENTIFIER { id }
  | DEFAULT         { Machine.Id.default }

machine_stanza:
  | s = id_stanza(MACHINE, id_or_default, machine_item) { s }

machine_item:
  | b = enabled         { Config_ast.Machine.Enabled b }
  | VIA; v = via_stanza { Config_ast.Machine.Via v }

via_stanza:
  | LOCAL                                { Config_ast.Via.Local }
  | items = simple_stanza(SSH, ssh_item) { Config_ast.Via.Ssh items }

ssh_item:
  | USER;     user    = STRING { Config_ast.Ssh.User    user }
  | HOST;     host    = STRING { Config_ast.Ssh.Host    host }
  | COPY; TO; copy_to = STRING { Config_ast.Ssh.Copy_to copy_to }

compiler_stanza:
  | s = id_stanza(COMPILER, IDENTIFIER, compiler_item) { s }
  (* Compilers don't have a 'default' identifier. *)

compiler_item:
  | b = enabled                    { Config_ast.Compiler.Enabled b }
  | c = cmd                        { Config_ast.Compiler.Cmd     c }
  | vs = argv                      { Config_ast.Compiler.Argv    vs }
  | STYLE;   style = IDENTIFIER    { Config_ast.Compiler.Style   style }
  | EMITS;   emits = IDENTIFIER    { Config_ast.Compiler.Emits   emits }
  | HERD;    on    = BOOL          { Config_ast.Compiler.Herd    on }
  | MACHINE; mach  = id_or_default { Config_ast.Compiler.Machine mach }

cmd:
  | CMD; c = STRING { c }

argv:
  | ARGV; vs = STRING+ { vs }

enabled:
  | ENABLED; b = BOOL { b }

