(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

%{
    open Ast
    open Ast_basic
%}

%token EOF
%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME (* TODO: lexer hack *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> CHAR_LIT (* string, not char, due to unicode *)
%token <string> STRING

%token LPAR "("
%token RPAR ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token COMMA ","
%token COLON ":"
%token DOT "."
%token DOTS ARROW QUESTION
%token SEMI ";"
%token CHAR INT SHORT LONG FLOAT DOUBLE VOID SIGNED UNSIGNED
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token CONST VOLATILE
%token STRUCT UNION ENUM
%token DO FOR WHILE IF ELSE
%token SWITCH CASE DEFAULT GOTO BREAK CONTINUE RETURN
%token SIZEOF

%token EQ "="
%token STAR_EQ DIV_EQ MOD_EQ ADD_EQ SUB_EQ SHL_EQ SHR_EQ AND_EQ XOR_EQ PIPE_EQ
%token LAND LOR LNOT
%token EQ_OP "=="
%token NEQ_OP "!="
%token LT LE GT GE

%token XOR PIPE AND SHR SHL NOT
%token ADDADD SUBSUB

%token ADD SUB
%token STAR DIV MOD

%token LIT_EXISTS LIT_FORALL LIT_LOCATIONS
%token LIT_TRUE LIT_FALSE
%token LIT_AND (* /\; not expressible as an alias *)
%token LIT_OR (* \/; not expressible as an alias *)

%type <Ast.Translation_unit.t> translation_unit
%start translation_unit

%type <(Ast.Litmus_lang.Constant.t, Ast.Litmus_lang.Program.t) Act_litmus.Ast.t> litmus
%start litmus

%type <Ast_basic.Constant.t Act_litmus.Postcondition.t> litmus_postcondition
%start litmus_postcondition

%%

%inline left_binop(this_level, next_level, op):
  | e = next_level { e }
  | l = this_level; o = op; r = next_level { Expr.Binary (l, o, r) }

%inline right_binop(this_level, next_level, op):
  | e = next_level { e }
  | l = next_level; o = op; r = this_level { Expr.Binary (l, o, r) }

let clist(x) == separated_list(",", x)
let nclist(x) == separated_nonempty_list(",", x)
let slist(x) == separated_list(";", x)
let braced(x) == delimited("{", x, "}")
let bracketed(x) == delimited("[", x, "]")
let parened(x) == delimited("(", x, ")")
let endsemi(x) == terminated(x, ";")

let litmus :=
  | language = IDENTIFIER; name = IDENTIFIER; decls = litmus_declaration+; EOF;
    { { Act_litmus.Ast.language = Act_common.C_id.of_string language
      ; name
      ; decls
      }
    }

let litmus_declaration :=
  | ~ = litmus_initialiser;   < Act_litmus.Ast.Decl.Init >
  | ~ = litmus_postcondition; < Act_litmus.Ast.Decl.Post >
  | ~ = litmus_locations;     < Act_litmus.Ast.Decl.Locations >
  | ~ = function_definition;  < Act_litmus.Ast.Decl.Program >

let litmus_init_stm :=
  | id = identifier; "="; value = constant; { { Act_litmus.Ast.Init.id; value } }

let litmus_initialiser := braced(list(endsemi(litmus_init_stm)))

let litmus_locations := LIT_LOCATIONS; bracketed(slist(identifier))

let litmus_quantifier :=
  | LIT_EXISTS; { Act_litmus.Postcondition.Quantifier.Exists }
  | LIT_FORALL; { Act_litmus.Postcondition.Quantifier.For_all }

let litmus_postcondition :=
  | quantifier = litmus_quantifier; predicate = parened(litmus_disjunct);
    { Act_litmus.Postcondition.make ~quantifier ~predicate }

let litmus_disjunct :=
  | litmus_conjunct
  | l = litmus_disjunct; LIT_OR; r = litmus_conjunct; { Act_litmus.Predicate.Infix.(l || r) }

let litmus_conjunct :=
  | litmus_primitive
  | l = litmus_conjunct; LIT_AND; r = litmus_primitive; { Act_litmus.Predicate.Infix.(l && r) }

let litmus_primitive :=
  | ~ = parened(litmus_disjunct); <>
  | LIT_TRUE; { Act_litmus.Predicate.bool true }
  | LIT_FALSE; { Act_litmus.Predicate.bool false }
  | l = litmus_identifier; "=="; r = constant; { Act_litmus.Predicate.Infix.(l ==? r) }

let litmus_identifier :=
  | i = IDENTIFIER;                     { Litmus.Id.global (Act_common.C_id.of_string i) }
  | t = INT_LIT; COLON; i = IDENTIFIER; { Litmus.Id.local t (Act_common.C_id.of_string i) }

translation_unit:
  | decls = external_declaration+ EOF { decls }

external_declaration:
  | func = function_definition { `Fun  func }
  | decl = declaration         { `Decl decl }

function_definition:
  | decl_specs = declaration_specifier*; signature = declarator; decls = declaration*; body = compound_statement
    { { Function_def.decl_specs; signature; decls; body } }

declaration:
  | qualifiers = declaration_specifier+; declarator = endsemi(clist(init_declarator))
    { { Decl.qualifiers; declarator } }

declaration_specifier:
  | spec = storage_class_specifier
  | spec = type_specifier
  | spec = type_qualifier
    { spec :> [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] }

storage_class_specifier:
  | AUTO     { `Auto }
  | REGISTER { `Register }
  | STATIC   { `Static }
  | EXTERN   { `Extern }
  | TYPEDEF  { `Typedef }

type_specifier:
  | VOID                          { `Void }
  | CHAR                          { `Char }
  | SHORT                         { `Short }
  | INT                           { `Int }
  | LONG                          { `Long }
  | FLOAT                         { `Float }
  | DOUBLE                        { `Double }
  | SIGNED                        { `Signed }
  | UNSIGNED                      { `Unsigned }
  | s = struct_or_union_specifier { `Struct_or_union s }
  | e = enum_specifier            { `Enum e }
  | t = TYPEDEF_NAME              { `Defined_type (Act_common.C_id.of_string t) }

type_qualifier:
  | CONST    { `Const }
  | VOLATILE { `Volatile }

struct_or_union_specifier:
  | kind = struct_or_union; name_opt = identifier?; decls = braced(struct_declaration+)
    { Struct_or_union_spec.Literal { kind; name_opt; decls } }
  | kind = struct_or_union; name = identifier
    { Struct_or_union_spec.Named ( kind, name ) }

struct_or_union:
  | STRUCT { `Struct }
  | UNION  { `Union }

init_declarator:
  | declarator = declarator; initialiser = option(preceded(EQ, initialiser))
    { { Init_declarator.declarator; initialiser } }

struct_declaration:
  | qualifiers = specifier_qualifier+; declarator = endsemi(nclist(struct_declarator))
    { { Struct_decl.qualifiers; declarator } }

specifier_qualifier:
  | spec = type_specifier
  | spec = type_qualifier
    { spec :> [ Type_spec.t | Type_qual.t ] }

struct_declarator:
  | decl = declarator
    { Struct_declarator.Regular decl }
  | decl = declarator?; COLON; length = expression
    { Struct_declarator.Bitfield (decl, length) }

enum_specifier:
  | ENUM; name_opt = identifier?; decls = braced(clist(enumerator))
    { Enum_spec.Literal { kind = `Enum; name_opt; decls } }
  | ENUM; name = identifier
    { Enum_spec.Literal { kind = `Enum; name_opt = Some name; decls = [] } }

enumerator:
  | name = identifier; value = option(preceded(EQ, constant_expression))
    { { Enumerator.name; value } }

declarator:
  | pointer = pointer?; direct = direct_declarator
    { { Declarator.pointer; direct } }

direct_declarator:
  | id = identifier
    { Direct_declarator.Id id }
  | d = parened(declarator)
    { Direct_declarator.Bracket d }
  | array = direct_declarator; index = bracketed(constant_expression?)
    { Direct_declarator.Array { Array.array; index } }
  | lhs = direct_declarator; pars = parened(parameter_type_list)
    { Direct_declarator.Fun_decl (lhs, pars) }
  | lhs = direct_declarator; pars = parened(clist(identifier))
    { Direct_declarator.Fun_call (lhs, pars) }

pointer:
  | xs = nonempty_list(preceded(STAR, type_qualifier*)) { xs }

parameter_type_list:
  | params = nclist(parameter_declaration); variadic = boption(preceded(COMMA, DOTS))
    { { Param_type_list.params
      ; style = if variadic then `Variadic else `Normal
      } }

let parameter_declarator :=
  | ~ = declarator;           < `Concrete >
  | ~ = abstract_declarator?; < `Abstract >

parameter_declaration:
  | qualifiers = declaration_specifier+; declarator = parameter_declarator
    { { Param_decl.qualifiers; declarator } }

initialiser:
  | expr = assignment_expression { Initialiser.Assign expr }
  | xs = braced(terminated(nclist(initialiser), COMMA?))
    { Initialiser.List xs }

type_name:
  | qualifiers = specifier_qualifier+; declarator = abstract_declarator?
    { { Type_name.qualifiers; declarator } }

abstract_declarator:
  | pointer = pointer
    { Abs_declarator.Pointer pointer }
  | pointer = pointer?; direct = direct_abstract_declarator
    { Abs_declarator.Direct (pointer, direct) }

direct_abstract_declarator:
  | d = parened(abstract_declarator)
    { Direct_abs_declarator.Bracket d }
  | array = direct_abstract_declarator?; index = bracketed(constant_expression?)
    { Direct_abs_declarator.Array { Array.array; index } }
  | lhs = direct_abstract_declarator?; pars = parened(parameter_type_list?)
    { Direct_abs_declarator.Fun_decl (lhs, pars) }

let statement :=
  | labelled_statement
  | expression_statement
  | ~ = compound_statement; < Stm.Compound >
  | selection_statement
  | iteration_statement
  | jump_statement

let labelled_statement :=
  | id = identifier; ":"; s = statement; { Stm.Label (Label.Normal id, s) }
  | CASE; cond = constant_expression; s = statement; { Stm.Label (Label.Case cond, s) }
  | DEFAULT; COLON; s = statement; { Stm.Label (Label.Default, s) }

expression_statement:
  | e = endsemi(expression?) { Stm.Expr e }

(* NB: this is (draft) C99.  Eventually, the rest of the grammar should be! *)

block_item:
  | decl = declaration { `Decl decl }
  | stm  = statement   { `Stm  stm  }

compound_statement:
  | LBRACE; items = block_item*; RBRACE
    { items }

selection_statement:
  | IF; cond = parened(expression); t_branch = statement; f_branch = option(preceded(ELSE, statement))
    { Stm.If { cond; t_branch; f_branch } }
  | SWITCH; cond = parened(expression); body = statement
    { Stm.Switch (cond, body) }

iteration_statement:
  | WHILE; cond = parened(expression); body = statement
    { Stm.While (cond, body) }
  | DO; body = statement; WHILE; cond = parened(expression)
    { Stm.Do_while (body, cond) }
  | FOR LPAR; init = expression?; SEMI; cond = expression?; SEMI; update = expression?; RPAR; body = statement
    { Stm.For { init; cond; update; body } }

jump_statement:
  | GOTO; id = identifier { Stm.Goto id }
  | CONTINUE { Stm.Continue }
  | BREAK { Stm.Break }
  | RETURN; ret = expression? { Stm.Return ret }

expression:
  | e = left_binop(expression, assignment_expression, COMMA {`Comma}) { e }

assignment_expression:
  | e = right_binop(assignment_expression, conditional_expression, assignment_operator) { e }
assignment_operator:
  | EQ      { `Assign }
  | STAR_EQ { `Assign_mul }
  | DIV_EQ  { `Assign_div }
  | MOD_EQ  { `Assign_mod }
  | ADD_EQ  { `Assign_add }
  | SUB_EQ  { `Assign_sub }
  | SHL_EQ  { `Assign_shl }
  | SHR_EQ  { `Assign_shr }
  | AND_EQ  { `Assign_and }
  | XOR_EQ  { `Assign_xor }
  | PIPE_EQ { `Assign_or }

conditional_expression:
  | e = logical_or_expression { e }
  | cond = logical_or_expression; QUESTION; t_expr = expression; COLON; f_expr = expression
    { Expr.Ternary { cond; t_expr; f_expr } }

constant_expression:
  | e = conditional_expression { e }

logical_or_expression:
  | e = left_binop(logical_or_expression, logical_and_expression, LOR { `Lor }) { e }

logical_and_expression:
  | e = left_binop(logical_and_expression, inclusive_or_expression, LAND { `Land }) { e }

inclusive_or_expression:
  | e = left_binop(inclusive_or_expression, exclusive_or_expression, PIPE { `Or }) { e }

exclusive_or_expression:
  | e = left_binop(exclusive_or_expression, and_expression, XOR { `Xor }) { e }

and_expression:
  | e = left_binop(and_expression, equality_expression, AND { `And }) { e }

equality_expression:
  | e = left_binop(equality_expression, relational_expression, equality_operator) { e }
let equality_operator :=
  | "=="; { `Eq }
  | "!="; { `Ne }

relational_expression:
  | e = left_binop(relational_expression, shift_expression, relational_operator) { e }
relational_operator:
  | LT { `Lt } (* < *)
  | LE { `Le } (* <= *)
  | GE { `Ge } (* >= *)
  | GT { `Gt } (* > *)

shift_expression:
  | e = left_binop(shift_expression, additive_expression, shift_operator) { e }
shift_operator:
  | SHL { `Shl } (* << *)
  | SHR { `Shr } (* >> *)

additive_expression:
  | e = left_binop(additive_expression, multiplicative_expression, additive_operator) { e }
additive_operator:
  | ADD { `Add } (* + *)
  | SUB { `Sub } (* - *)

multiplicative_expression:
  | e = left_binop(multiplicative_expression, cast_expression, multiplicative_operator) { e }
multiplicative_operator:
  | STAR { `Mul } (* * *)
  | DIV  { `Div } (* / *)
  | MOD  { `Mod } (* % *)

cast_expression:
  | e = unary_expression { e }
  | ty = parened(type_name); e = cast_expression { Expr.Cast (ty, e) }

inc_or_dec_operator:
  | ADDADD { `Inc } (* ++ *)
  | SUBSUB { `Dec } (* -- *)

unary_operator_unary:
  | o = inc_or_dec_operator { o :> Operators.Pre.t }
  | SIZEOF { `Sizeof_val }

unary_operator_cast:
  | AND  { `Ref }
  | STAR { `Deref }
  | ADD  { `Add }
  | SUB  { `Sub }
  | NOT  { `Not }
  | LNOT { `Lnot }

unary_expression:
  | e = postfix_expression { e }
  | o = unary_operator_unary; e = unary_expression { Expr.Prefix (o, e) }
  | o = unary_operator_cast; e = cast_expression { Expr.Prefix ((o :> Operators.Pre.t), e) }
  | SIZEOF; ty = parened(type_name) { Expr.Sizeof_type (ty) }

field_access:
  | DOT   { `Direct } (* . *)
  | ARROW { `Deref }  (* -> *)

postfix_expression:
  | e = primary_expression { e }
  | array = postfix_expression; index = expression
    { Expr.Subscript { array; index } }
  | func = postfix_expression; arguments = parened(argument_expression_list)
    { Expr.Call { func; arguments } }
  | value = postfix_expression; access = field_access; field = identifier
    { Expr.Field { value; field; access } }
  | e = postfix_expression; o = inc_or_dec_operator
    { Expr.Postfix (e, o) }

(* We can't use clist here; it produces a reduce-reduce conflict. *)
argument_expression_list:
  | x = assignment_expression { [x] }
  | x = assignment_expression; COMMA; xs = argument_expression_list { x::xs }

primary_expression:
  | i = identifier          { Expr.Identifier i }
  | k = constant            { Expr.Constant   k }
  | s = STRING              { Expr.String     s }
  | e = parened(expression) { Expr.Brackets   e }

constant:
  | i = INT_LIT   { Constant.Integer i }
  | c = CHAR_LIT  { Constant.Char    c }
  | f = FLOAT_LIT { Constant.Float   f }

identifier:
(* The lexer should do C identifier validation for us by construction. *)
  | i = IDENTIFIER { Act_common.C_id.of_string i }
(* Contextual keywords. *)
  | LIT_TRUE { Act_common.C_id.of_string "true" }
  | LIT_FALSE { Act_common.C_id.of_string "false" }
  | LIT_EXISTS { Act_common.C_id.of_string "exists" }
  | LIT_FORALL { Act_common.C_id.of_string "forall" }
  | LIT_LOCATIONS { Act_common.C_id.of_string "locations" }
