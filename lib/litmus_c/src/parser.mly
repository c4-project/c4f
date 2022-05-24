(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* This file contains a Menhir parser for Litmus C.

   The Litmus C dialect supported by ACT supports most of C89, with quirks:

   - There isn't any C preprocessor support;
   - There isn't any proper typedef support, to avoid implementing the lexer
     hack; instead, the parser gets sent a list of statically determined
     'known typedefs';
   - We support transactions using Memalloy's syntax ('atomic', 'synchronized',
     etc);
   - We support mixed declarations and code;
   - While it doesn't appear in the grammar per se, we support C11 atomics.
*)

%{
    open Ast
    open Ast_basic
%}

%token EOF
%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME (* see above: we don't support user typedefs. *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> CHAR_LIT (* string, not char, due to unicode *)
%token <string> STRING

%token CHAR INT SHORT LONG FLOAT DOUBLE VOID SIGNED UNSIGNED
%token AUTO REGISTER STATIC EXTERN
%token CONST VOLATILE
%token STRUCT UNION ENUM
%token DO FOR WHILE IF ELSE
%token SWITCH CASE DEFAULT GOTO BREAK CONTINUE RETURN
%token SIZEOF

(* Types of transaction block *)
%token ATOMIC SYNCHRONIZED

%token LPAR     "("
%token RPAR     ")"
%token LBRACE   "{"
%token RBRACE   "}"
%token LBRACK   "["
%token RBRACK   "]"

%token COMMA    ","
%token COLON    ":"
%token DOT      "."
%token DOTS     "..."
%token ARROW    "->"
%token QUESTION "?"
%token SEMI     ";"

%token EQ      "="
%token STAR_EQ "*="
%token DIV_EQ  "/="
%token MOD_EQ  "%="
%token ADD_EQ  "+="
%token SUB_EQ  "-="
%token SHL_EQ  "<<="
%token SHR_EQ  ">>="
%token AND_EQ  "&="
%token XOR_EQ  "^="
%token PIPE_EQ "|="

%token LAND   "&&"
%token LOR    "||"
%token LNOT   "!"
%token EQ_OP  "=="
%token NEQ_OP "!="

%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="

%token XOR  "^"
%token PIPE "|"
%token AND  "&"
%token SHR  ">>"
%token SHL  "<<"
%token NOT  "~"

%token ADDADD "++"
%token SUBSUB "--"

%token ADD  "+"
%token SUB  "-"
%token STAR "*"
%token DIV  "/"
%token MOD  "%"

(* Litmus tokens *)
%token LIT_EXISTS LIT_FORALL LIT_LOCATIONS
%token LIT_TRUE LIT_FALSE
%token LIT_AND (* /\; not expressible as an alias *)
%token LIT_OR (* \/; not expressible as an alias *)

(* Endpoint for 'standalone' translation units, not contained within a test. *)
%start <Ast.Translation_unit.t> translation_unit

(* Endpoint for Litmus tests. *)
%start <(Ast.Litmus_lang.Constant.t, Ast.Litmus_lang.Program.t) C4f_litmus.Ast.t> litmus

(* Endpoint for standalone Litmus postconditions. *)
%start <Ast_basic.Constant.t C4f_litmus.Postcondition.t> litmus_postcondition

%%

let left_binop(this_level, next_level, op) ==
  | next_level
  | ~ = this_level; ~ = op; ~ = next_level; < Expr.Binary >

let right_binop(this_level, next_level, op) ==
  | next_level
  | ~ = next_level; ~ = op; ~ = this_level; < Expr.Binary >

let clist(x) == separated_list(",", x)
let nclist(x) == separated_nonempty_list(",", x)
let slist(x) == separated_list(";", x)
let braced(x) == delimited("{", x, "}")
let bracketed(x) == delimited("[", x, "]")
let parened(x) == delimited("(", x, ")")
let endsemi(x) == terminated(x, ";")

(*
 * Litmus specifics
 *)

let litmus :=
  | language = IDENTIFIER; name = IDENTIFIER; decls = litmus_declaration+; EOF;
    { { C4f_litmus.Ast.language = C4f_common.C_id.of_string language
      ; name
      ; decls
      }
    }

let litmus_declaration :=
  | ~ = litmus_initialiser;   < C4f_litmus.Ast.Decl.Init >
  | ~ = litmus_postcondition; < C4f_litmus.Ast.Decl.Post >
  | ~ = litmus_locations;     < C4f_litmus.Ast.Decl.Locations >
  | ~ = function_definition;  < C4f_litmus.Ast.Decl.Program >

let litmus_init_stm :=
  | id = c_identifier; "="; value = constant; { { C4f_litmus.Ast.Init.id; value } }

let litmus_initialiser := braced(list(endsemi(litmus_init_stm)))

let litmus_locations := LIT_LOCATIONS; bracketed(slist(c_identifier))

let litmus_quantifier :=
  | LIT_EXISTS; { C4f_litmus.Postcondition.Quantifier.Exists }
  | LIT_FORALL; { C4f_litmus.Postcondition.Quantifier.For_all }

let litmus_postcondition :=
  | quantifier = litmus_quantifier; predicate = parened(litmus_disjunct);
    { C4f_litmus.Postcondition.make ~quantifier ~predicate }

let litmus_disjunct :=
  | litmus_conjunct
  | l = litmus_disjunct; LIT_OR; r = litmus_conjunct; { C4f_litmus.Predicate.Infix.(l || r) }

let litmus_conjunct :=
  | litmus_primitive
  | l = litmus_conjunct; LIT_AND; r = litmus_primitive; { C4f_litmus.Predicate.Infix.(l && r) }

let litmus_primitive :=
  | ~ = parened(litmus_disjunct); <>
  | LIT_TRUE; { C4f_litmus.Predicate.bool true }
  | LIT_FALSE; { C4f_litmus.Predicate.bool false }
  | l = litmus_identifier; "=="; r = constant; { C4f_litmus.Predicate.Infix.(l ==? r) }

let litmus_identifier :=
  | i = IDENTIFIER;                   { Litmus.Id.global (C4f_common.C_id.of_string i) }
  | t = INT_LIT; ":"; i = IDENTIFIER; { Litmus.Id.local t (C4f_common.C_id.of_string i) }

(*
 * C grammar
 *)

let translation_unit := ~ = external_declaration+; EOF; <>

let external_declaration :=
  | ~ = function_definition; < `Fun  >
  | ~ = declaration        ; < `Decl >

let function_definition :=
  | decl_specs = declaration_specifier*; signature = declarator; decls = declaration*; body = compound_statement;
    { { Function_def.decl_specs; signature; decls; body } }

let declaration :=
  | qualifiers = declaration_specifier+; declarator = endsemi(clist(init_declarator));
    { { Decl.qualifiers; declarator } }

let declaration_specifier :=
  | spec = storage_class_specifier;
    { spec :> [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] }
  | spec = type_specifier;
    { spec :> [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] }
  | spec = type_qualifier;
    { spec :> [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] }

let storage_class_specifier :=
  | AUTO    ; { `Auto }
  | REGISTER; { `Register }
  | STATIC  ; { `Static }
  | EXTERN  ; { `Extern }
  (* C89 supports 'typedef' here, but we don't. *)

let type_specifier :=
  | VOID                         ; { `Void }
  | CHAR                         ; { `Char }
  | SHORT                        ; { `Short }
  | INT                          ; { `Int }
  | LONG                         ; { `Long }
  | FLOAT                        ; { `Float }
  | DOUBLE                       ; { `Double }
  | SIGNED                       ; { `Signed }
  | UNSIGNED                     ; { `Unsigned }
  | ~ = struct_or_union_specifier; < `Struct_or_union >
  | ~ = enum_specifier           ; < `Enum >
  (* Statically known typedefs only. *)
  | t = TYPEDEF_NAME             ; { `Defined_type (C4f_common.C_id.of_string t) }

let type_qualifier :=
  | CONST   ; { `Const }
  | VOLATILE; { `Volatile }

let struct_or_union_specifier :=
  | kind = struct_or_union; name_opt = c_identifier?; decls = braced(struct_declaration+);
    { Struct_or_union_spec.Literal { kind; name_opt; decls } }
  | ~ = struct_or_union; ~ = c_identifier; < Struct_or_union_spec.Named >

let struct_or_union :=
  | STRUCT; { `Struct }
  | UNION ; { `Union }

let init_declarator :=
  declarator = declarator; initialiser = option(preceded(EQ, initialiser));
    { { Init_declarator.declarator; initialiser } }

let struct_declaration :=
  qualifiers = specifier_qualifier+; declarator = endsemi(nclist(struct_declarator));
    { { Struct_decl.qualifiers; declarator } }

let specifier_qualifier :=
  | spec = type_specifier;
    { spec :> [ Type_spec.t | Type_qual.t ] }
  | spec = type_qualifier;
    { spec :> [ Type_spec.t | Type_qual.t ] }

let struct_declarator :=
  | ~ = declarator                      ; < Struct_declarator.Regular >
  | ~ = declarator?; ":"; ~ = expression; < Struct_declarator.Bitfield >

let enum_specifier :=
  | ENUM; name_opt = c_identifier?; decls = braced(clist(enumerator));
    { Enum_spec.Literal { kind = `Enum; name_opt; decls } }
  | ENUM; name = c_identifier;
    { Enum_spec.Literal { kind = `Enum; name_opt = Some name; decls = [] } }

let enumerator :=
  name = c_identifier; value = option(preceded(EQ, constant_expression));
    { { Enumerator.name; value } }

let declarator :=
  pointer = pointer?; direct = direct_declarator;
    { { Declarator.pointer; direct } }

let direct_declarator :=
(* Litmus/C restriction *)
  | ~ = restricted_c_identifier;
    < Direct_declarator.Id >
  | ~ = parened(declarator);
    < Direct_declarator.Bracket >
  | array = direct_declarator; index = bracketed(constant_expression?);
    { Direct_declarator.Array { Array.array; index } }
  | ~ = direct_declarator; ~ = parened(parameter_type_list);
    < Direct_declarator.Fun_decl >
  | ~ = direct_declarator; ~ = parened(clist(c_identifier));
    < Direct_declarator.Fun_call >

let pointer := nonempty_list(preceded(STAR, type_qualifier*))

let parameter_type_list :=
  params = nclist(parameter_declaration); variadic = boption(preceded(",", "..."));
    { { Param_type_list.params
      ; style = if variadic then `Variadic else `Normal
      } }

let parameter_declarator :=
  | ~ = declarator;           < `Concrete >
  | ~ = abstract_declarator?; < `Abstract >

let parameter_declaration :=
  | qualifiers = declaration_specifier+; declarator = parameter_declarator;
    { { Param_decl.qualifiers; declarator } }

let initialiser :=
  | ~ = assignment_expression                          ; < Initialiser.Assign >
  | ~ = braced(terminated(nclist(initialiser), COMMA?)); < Initialiser.List >

let type_name :=
  | qualifiers = specifier_qualifier+; declarator = abstract_declarator?;
    { { Type_name.qualifiers; declarator } }

let abstract_declarator :=
  | ~ = pointer; < Abs_declarator.Pointer >
  | ~ = pointer?; ~ = direct_abstract_declarator; < Abs_declarator.Direct >

direct_abstract_declarator:
  | d = parened(abstract_declarator)
    { Direct_abs_declarator.Bracket d }
  | array = direct_abstract_declarator?; index = bracketed(constant_expression?)
    { Direct_abs_declarator.Array { Array.array; index } }
  | lhs = direct_abstract_declarator?; pars = parened(parameter_type_list?)
    { Direct_abs_declarator.Fun_decl (lhs, pars) }

(* Instead of the classic K&R statement derivation, we use the 'open/closed'
   statement setup (eg http://www.parsifalsoft.com/ifelse.html) to remove the
   ambiguity of `if (c1) if (c2) foo; else bar;`.
   
   Instead of duplicating every production that can produce either an open or
   closed statement depending on whether it's in one, we parametrise the rules
   by the appropriate class of statement. *)

let statement := open_statement | closed_statement

let open_statement :=
  | one_selection_statement
  | statement_common(open_statement)

let closed_statement :=
  | simple_statement
  | statement_common(closed_statement)

let statement_common(s) ==
  | two_selection_statement(s)
  | iteration_statement(s)
  | labelled_statement(s)

let simple_statement :=
  | ~ = compound_statement; < Stm.Compound >
  | expression_statement
  | jump_statement
(* Litmus/C extension *)
  | tx_statement

let labelled_statement(s) := ~ = label; ":"; ~ = s; < Stm.Label >
let label :=
  | ~ = c_identifier;              < Label.Normal >
  | CASE; ~ = constant_expression; < Label.Case >
  | DEFAULT;                       { Label.Default }

let expression_statement := ~ = endsemi(expression?); < Stm.Expr >

let compound_statement := braced(block_item*)
let block_item :=
  (* NB: this is (draft) C99.  Eventually, the rest of the grammar should be! *)
  | ~ = declaration; < `Decl >
  | ~ = statement  ; < `Stm  >

let one_selection_statement :=
  cond = if_clause; t_branch = statement; { Stm.If { cond; t_branch; f_branch=None }}

let two_selection_statement(s) ==
  | cond = if_clause; t_branch = closed_statement; ELSE; f_branch = s;
    { Stm.If { cond; t_branch; f_branch = Some (f_branch) } }
  | SWITCH; ~ = parened(expression); ~ = s; < Stm.Switch >

let if_clause := preceded(IF, parened(expression))

let iteration_statement(s) :=
  | WHILE; ~ = parened(expression); ~ = s;
    < Stm.While >
  | DO; ~ = s; WHILE; ~ = parened(expression);
    < Stm.Do_while >
  | FOR; "("; init = expression?; ";"; cond = expression?; ";"; update = expression?; ")"; body = s;
    { Stm.For { init; cond; update; body } }

let jump_statement :=
  | GOTO; id = c_identifier; < Stm.Goto >
  | CONTINUE               ; { Stm.Continue }
  | BREAK                  ; { Stm.Break }
  | RETURN; ~ = expression?; < Stm.Return >

(* Litmus/C extension: transaction blocks. *)
let tx_statement :=
  | ATOMIC      ; ~ = compound_statement; < Stm.Atomic >
  | SYNCHRONIZED; ~ = compound_statement; < Stm.Synchronized >

(* Expressions follow the hand-stratified grammar seen in the back of K&R.
   TODO(@MattWindsor91): eventually work out how to replace this with an
   automatically stratfied version. *)
let expression :=
  left_binop(expression, assignment_expression, ","; {`Comma})

let assignment_expression :=
  right_binop(assignment_expression, conditional_expression, assignment_operator)
let assignment_operator :=
  | "="  ; { `Assign }
  | "*=" ; { `Assign_mul }
  | "/=" ; { `Assign_div }
  | "%=" ; { `Assign_mod }
  | "+=" ; { `Assign_add }
  | "-=" ; { `Assign_sub }
  | "<<="; { `Assign_shl }
  | ">>="; { `Assign_shr }
  | "&=" ; { `Assign_and }
  | "^=" ; { `Assign_xor }
  | "|=" ; { `Assign_or }

let conditional_expression :=
  | logical_or_expression
  | cond = logical_or_expression; "?"; t_expr = expression; ":"; f_expr = conditional_expression;
    { Expr.Ternary { cond; t_expr; f_expr } }

let constant_expression := conditional_expression

let logical_or_expression :=
  left_binop(logical_or_expression, logical_and_expression, "||"; { `Lor })

let logical_and_expression :=
  left_binop(logical_and_expression, inclusive_or_expression, "&&"; { `Land })

let inclusive_or_expression :=
  left_binop(inclusive_or_expression, exclusive_or_expression, "|"; { `Or })

let exclusive_or_expression :=
  left_binop(exclusive_or_expression, and_expression, "^"; { `Xor })

let and_expression :=
  left_binop(and_expression, equality_expression, "&"; { `And })

let equality_expression :=
  left_binop(equality_expression, relational_expression, equality_operator)
let equality_operator := "=="; { `Eq } | "!="; { `Ne }

let relational_expression :=
  | left_binop(relational_expression, shift_expression, relational_operator)
let relational_operator :=
  "<" ; { `Lt } | "<="; { `Le } | ">="; { `Ge } | ">" ; { `Gt }

let shift_expression :=
  left_binop(shift_expression, additive_expression, shift_operator)
let shift_operator := "<<"; { `Shl } | ">>"; { `Shr }

let additive_expression :=
  left_binop(additive_expression, multiplicative_expression, additive_operator)
let additive_operator := "+"; { `Add } | "-"; { `Sub }

let multiplicative_expression :=
  left_binop(multiplicative_expression, cast_expression, multiplicative_operator)
let multiplicative_operator := "*"; { `Mul } | "/"; { `Div } | "%"; { `Mod }

let cast_expression :=
  | unary_expression
  | ~ = parened(type_name); ~ = cast_expression; < Expr.Cast >

let unary_expression :=
  | postfix_expression
  | ~ = unary_operator_unary; ~ = unary_expression; < Expr.Prefix >
  | o = unary_operator_cast; e = cast_expression; { Expr.Prefix ((o :> Operators.Pre.t), e) }
  | SIZEOF; ~ = parened(type_name); < Expr.Sizeof_type >
let unary_operator_unary :=
  | o = inc_or_dec_operator; { o :> Operators.Pre.t }
  | SIZEOF; { `Sizeof_val }
let inc_or_dec_operator := "++"; { `Inc } | "--"; { `Dec }
let unary_operator_cast :=
  | "&"; { `Ref }
  | "*"; { `Deref }
  | "+"; { `Add }
  | "-"; { `Sub }
  | "~"; { `Not }
  | "!"; { `Lnot }

let postfix_expression :=
  | primary_expression
  | array = postfix_expression; index = bracketed(expression);
    { Expr.Subscript { array; index } }
  | func = postfix_expression; arguments = parened(clist(assignment_expression));
    { Expr.Call { func; arguments } }
  | value = postfix_expression; access = field_access; field = c_identifier;
    { Expr.Field { value; field; access } }
  | ~ = postfix_expression; ~ = inc_or_dec_operator;
    <Expr.Postfix>
let field_access := "." ; { `Direct } | "->"; { `Deref }

let primary_expression :=
  | ~ = c_identifier       ; < Expr.Identifier >
  | ~ = constant           ; < Expr.Constant   >
  | ~ = STRING             ; < Expr.String     >
  | ~ = parened(expression); < Expr.Brackets   >

let constant :=
  | ~ = INT_LIT  ; < Constant.Integer >
  | ~ = CHAR_LIT ; < Constant.Char    >
  | ~ = FLOAT_LIT; < Constant.Float   >

let c_identifier :=
  | restricted_c_identifier
  | ~ = keyword_as_identifier; < C4f_common.C_id.of_string >

let keyword_as_identifier :=
(* Contextual keywords that aren't in keyword position, and are therefore just
   emitted as their identifiers. *)
  | LIT_TRUE     ; { "true" }
  | LIT_FALSE    ; { "false" }
  | LIT_EXISTS   ; { "exists" }
  | LIT_FORALL   ; { "forall" }
  | LIT_LOCATIONS; { "locations" }

let restricted_c_identifier :=
(* The lexer should do C identifier validation for us by construction. *)
  | ~ = IDENTIFIER; < C4f_common.C_id.of_string >
