%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

%}

%token EOF
%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME (* TODO: lexer hack *)
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING

%token LPAR RPAR LBRACE RBRACE LBRACK RBRACK
%token COMMA COLON DOT DOTS ARROW QUESTION
%token CHAR INT SHORT LONG FLOAT DOUBLE VOID SIGNED UNSIGNED
%token AUTO REGISTER STATIC EXTERN TYPEDEF
%token CONST VOLATILE
%token STRUCT UNION ENUM
%token DO FOR WHILE IF ELSE
%token SWITCH CASE DEFAULT GOTO BREAK CONTINUE RETURN
%token SIZEOF
%token SEMI

%token EQ STAR_EQ DIV_EQ MOD_EQ ADD_EQ SUB_EQ SHL_EQ SHR_EQ AND_EQ XOR_EQ PIPE_EQ
%token LAND LOR LNOT
%token EQ_OP NEQ_OP
%token LT LE GT GE

%token XOR PIPE AND SHR SHL NOT
%token ADDADD SUBSUB

%token ADD SUB
%token STAR DIV MOD

%token LIT_EXISTS LIT_AND LIT_OR

%type <Ast.Translation_unit.t> translation_unit
%start translation_unit

%type <Ast.Litmus.t> litmus
%start litmus

%%

%inline clist(x):
  | xs = separated_list(COMMA, x) { xs }

%inline nclist(x):
  | xs = separated_nonempty_list(COMMA, x) { xs }

%inline slist(x):
  | xs = separated_list(SEMI, x) { xs }

%inline nslist(x):
  | xs = separated_nonempty_list(SEMI, x) { xs }

%inline braced(x):
  | xs = delimited(LBRACE, x, RBRACE) { xs }

%inline bracketed(x):
  | xs = delimited(LBRACK, x, RBRACK) { xs }

%inline parened(x):
  | xs = delimited(LPAR, x, RPAR) { xs }

%inline endsemi(x):
  | xs = terminated(x, SEMI) { xs }

litmus:
  | language = IDENTIFIER; name = IDENTIFIER; decls = litmus_declaration+; EOF
    { { Ast.Litmus.language; name; decls } }

litmus_declaration:
  | decl = external_declaration { decl :> Ast.Litmus_decl.t }
  | decl = litmus_initialiser   { `Init decl }
  | post = litmus_postcondition { `Post post }

litmus_initialiser:
  | xs = braced(list(endsemi(assignment_expression))) { xs }

litmus_quantifier:
  | LIT_EXISTS { `Exists }

litmus_postcondition:
  | quantifier = litmus_quantifier; predicate = parened(litmus_disjunct)
    { { Ast.Litmus_post.quantifier; predicate } }

litmus_disjunct:
  | e = litmus_conjunct { e }
  | l = litmus_disjunct; LIT_OR; r = litmus_conjunct { Ast.Litmus_pred.Or (l, r) }

litmus_conjunct:
  | e = litmus_equality { e }
  | l = litmus_conjunct; LIT_AND; r = litmus_equality { Ast.Litmus_pred.And (l, r) }

litmus_equality:
  | e = parened(litmus_disjunct) { Ast.Litmus_pred.Bracket (e) }
  | l = IDENTIFIER; EQ_OP; r = constant { Ast.Litmus_pred.Eq (l, r) }

translation_unit:
  | decls = external_declaration+ EOF { decls }

external_declaration:
  | func = function_definition { `Fun  func }
  | decl = declaration         { `Decl decl }

function_definition:
  | decl_specs = declaration_specifier*; signature = declarator; decls = declaration*; body = compound_statement
    { { Ast.Function_def.decl_specs; signature; decls; body } }

declaration:
  | qualifiers = declaration_specifier+; declarator = endsemi(clist(init_declarator))
    { { Ast.Decl.qualifiers; declarator } }

declaration_specifier:
  | spec = storage_class_specifier { spec :> Ast.Decl_spec.t }
  | spec = type_specifier          { spec :> Ast.Decl_spec.t }
  | qual = type_qualifier          { qual :> Ast.Decl_spec.t }

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
  | t = TYPEDEF_NAME              { `Defined_type t }

type_qualifier:
  | CONST    { `Const }
  | VOLATILE { `Volatile }

struct_or_union_specifier:
  | ty = struct_or_union; name_opt = identifier?; decls = braced(struct_declaration+)
    { Ast.Struct_or_union_spec.Literal { ty; name_opt; decls } }
  | ty = struct_or_union; name = identifier
    { Ast.Struct_or_union_spec.Named ( ty, name ) }

struct_or_union:
  | STRUCT { `Struct }
  | UNION  { `Union }

init_declarator:
  | declarator = declarator; initialiser = option(preceded(EQ, initialiser))
    { { Ast.Init_declarator.declarator; initialiser } }

struct_declaration:
  | qualifiers = specifier_qualifier+; declarator = endsemi(nclist(struct_declarator))
    { { Ast.Struct_decl.qualifiers; declarator } }

specifier_qualifier:
  | spec = type_specifier { spec :> [ Ast.Type_spec.t | Ast.Type_qual.t ] }
  | qual = type_qualifier { qual :> [ Ast.Type_spec.t | Ast.Type_qual.t ] }

struct_declarator:
  | decl = declarator
    { Ast.Struct_declarator.Regular decl }
  | decl = declarator?; COLON; length = expression
    { Ast.Struct_declarator.Bitfield (decl, length) }

enum_specifier:
  | ENUM; name_opt = identifier?; decls = braced(clist(enumerator))
    { Ast.Enum_spec.Literal { ty = `Enum; name_opt; decls } }
  | ENUM; name = identifier
    { Ast.Enum_spec.Literal { ty = `Enum; name_opt = Some name; decls = [] } }

enumerator:
  | name = identifier; value = option(preceded(EQ, constant_expression))
    { { Ast.Enumerator.name; value } }

declarator:
  | pointer = pointer?; direct = direct_declarator
    { { Ast.Declarator.pointer; direct } }

direct_declarator:
  | id = identifier
    { Ast.Direct_declarator.Id id }
  | d = parened(declarator)
    { Ast.Direct_declarator.Bracket d }
  | lhs = direct_declarator; index = bracketed(constant_expression?)
    { Ast.Direct_declarator.Array (lhs, index) }
  | lhs = direct_declarator; pars = parened(parameter_type_list)
    { Ast.Direct_declarator.Fun_decl (lhs, pars) }
  | lhs = direct_declarator; pars = parened(clist(identifier))
    { Ast.Direct_declarator.Fun_call (lhs, pars) }

pointer:
  | xs = nonempty_list(preceded(STAR, type_qualifier*)) { xs }

parameter_type_list:
  | params = nclist(parameter_declaration); variadic = boption(preceded(COMMA, DOTS))
    { { Ast.Param_type_list.params
      ; style = if variadic then `Variadic else `Normal
      } }

parameter_declarator:
  | d = declarator           { `Concrete d }
  | d = abstract_declarator? { `Abstract d }

parameter_declaration:
  | qualifiers = declaration_specifier+; declarator = parameter_declarator
    { { Ast.Param_decl.qualifiers; declarator } }

initialiser:
  | expr = assignment_expression { Ast.Initialiser.Assign expr }
  | xs = braced(terminated(nclist(initialiser), COMMA?))
    { Ast.Initialiser.List xs }

type_name:
  | qualifiers = specifier_qualifier+; declarator = abstract_declarator?
    { { Ast.Type_name.qualifiers; declarator } }

abstract_declarator:
  | pointer = pointer
    { Ast.Abs_declarator.Pointer pointer }
  | pointer = pointer?; direct = direct_abstract_declarator
    { Ast.Abs_declarator.Direct (pointer, direct) }

direct_abstract_declarator:
  | d = parened(abstract_declarator)
    { Ast.Direct_abs_declarator.Bracket d }
  | lhs = direct_abstract_declarator?; index = bracketed(constant_expression?)
    { Ast.Direct_abs_declarator.Array (lhs, index) }
  | lhs = direct_abstract_declarator?; pars = parened(parameter_type_list?)
    { Ast.Direct_abs_declarator.Fun_decl (lhs, pars) }

statement:
  | s = labelled_statement   { s }
  | s = expression_statement { s }
  | s = compound_statement   { Ast.Stm.Compound s }
  | s = selection_statement  { s }
  | s = iteration_statement  { s }
  | s = jump_statement       { s }

labelled_statement:
  | id = identifier; COLON; s = statement { Ast.Stm.Label (Ast.Label.Normal id, s) }
  | CASE; cond = constant_expression; s = statement { Ast.Stm.Label (Ast.Label.Case cond, s) }
  | DEFAULT; COLON; s = statement { Ast.Stm.Label (Ast.Label.Default, s) }

expression_statement:
  | e = endsemi(expression?) { Ast.Stm.Expr e }

compound_statement:
  | LBRACE; decls = declaration*; stms = statement*; RBRACE
    { { Ast.Compound_stm.decls; stms } }

selection_statement:
  | IF; cond = parened(expression); t_branch = statement; f_branch = option(preceded(ELSE, statement))
    { Ast.Stm.If { cond; t_branch; f_branch } }
  | SWITCH; cond = parened(expression); body = statement
    { Ast.Stm.Switch (cond, body) }

iteration_statement:
  | WHILE; cond = parened(expression); body = statement
    { Ast.Stm.While (cond, body) }
  | DO; body = statement; WHILE; cond = parened(expression)
    { Ast.Stm.Do_while (body, cond) }
  | FOR LPAR; init = expression?; SEMI; cond = expression?; SEMI; update = expression?; RPAR; body = statement
    { Ast.Stm.For { init; cond; update; body } }

jump_statement:
  | GOTO; id = identifier { Ast.Stm.Goto id }
  | CONTINUE { Ast.Stm.Continue }
  | BREAK { Ast.Stm.Break }
  | RETURN; ret = expression? { Ast.Stm.Return ret }

expression:
  | e = assignment_expression { e }
  | l = expression; COMMA; r = assignment_expression
    { Ast.Expr.Binary (l, `Comma, r) }

assignment_expression:
  | e = conditional_expression { e }
  | l = unary_expression; o = assignment_operator; r = assignment_expression
    { Ast.Expr.Binary (l, o, r) }

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
    { Ast.Expr.Ternary { cond; t_expr; f_expr } }

constant_expression:
  | e = conditional_expression { e }

logical_or_expression:
  | e = logical_and_expression { e }
  | l = logical_or_expression; LOR; r = logical_and_expression
    { Ast.Expr.Binary (l, `Lor, r) }

logical_and_expression:
  | e = inclusive_or_expression { e }
  | l = logical_and_expression; LAND; r = inclusive_or_expression
    { Ast.Expr.Binary (l, `Land, r) }

inclusive_or_expression:
  | e = exclusive_or_expression { e }
  | l = inclusive_or_expression; PIPE; r = exclusive_or_expression
    { Ast.Expr.Binary (l, `Or, r) }

exclusive_or_expression:
  | e = and_expression { e }
  | l = exclusive_or_expression; XOR; r = and_expression
    { Ast.Expr.Binary (l, `Xor, r) }

and_expression:
  | e = equality_expression { e }
  | l = and_expression; AND; r = equality_expression
    { Ast.Expr.Binary (l, `And, r) }

equality_op:
  | EQ_OP  { `Eq } (* == *)
  | NEQ_OP { `Ne } (* != *)

equality_expression:
  | e = relational_expression { e }
  | l = equality_expression; o = equality_op; r = relational_expression
    { Ast.Expr.Binary (l, o, r) }

relational_op:
  | LT { `Lt } (* < *)
  | LE { `Le } (* <= *)
  | GE { `Ge } (* >= *)
  | GT { `Gt } (* > *)

relational_expression:
  | e = shift_expression { e }
  | l = relational_expression; o = relational_op; r = shift_expression
    { Ast.Expr.Binary (l, o, r) }

shift_op:
  | SHL { `Shl } (* << *)
  | SHR { `Shr } (* >> *)

shift_expression:
  | e = additive_expression { e }
  | l = shift_expression; o = shift_op; r = additive_expression
    { Ast.Expr.Binary (l, o, r) }

additive_op:
  | ADD { `Add } (* + *)
  | SUB { `Sub } (* - *)

additive_expression:
  | e = multiplicative_expression { e }
  | l = additive_expression; o = additive_op; r = multiplicative_expression
    { Ast.Expr.Binary (l, o, r) }

multiplicative_op:
  | STAR { `Mul } (* * *)
  | DIV  { `Div } (* / *)
  | MOD  { `Mod } (* % *)

multiplicative_expression:
  | e = cast_expression { e }
  | l = multiplicative_expression; o = multiplicative_op; r = cast_expression
    { Ast.Expr.Binary (l, o, r) }

cast_expression:
  | e = unary_expression { e }
  | ty = parened(type_name); e = cast_expression { Ast.Expr.Cast (ty, e) }

inc_or_dec_operator:
  | ADDADD { `Inc } (* ++ *)
  | SUBSUB { `Dec } (* -- *)

unary_operator_unary:
  | o = inc_or_dec_operator { o :> Ast.Operator.pre }
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
  | o = unary_operator_unary; e = unary_expression { Ast.Expr.Prefix (o, e) }
  | o = unary_operator_cast; e = cast_expression { Ast.Expr.Prefix ((o :> Ast.Operator.pre), e) }
  | SIZEOF; ty = parened(type_name) { Ast.Expr.Sizeof_type (ty) }

field_access:
  | DOT   { `Direct } (* . *)
  | ARROW { `Deref }  (* -> *)

postfix_expression:
  | e = primary_expression { e }
  | array = postfix_expression; index = expression
    { Ast.Expr.Subscript { array; index } }
  | func = postfix_expression; arguments = parened(argument_expression_list)
    { Ast.Expr.Call { func; arguments } }
  | value = postfix_expression; access = field_access; field = identifier
    { Ast.Expr.Field { value; field; access } }
  | e = postfix_expression; o = inc_or_dec_operator
    { Ast.Expr.Postfix (e, o) }

(* We can't use clist here; it produces a reduce-reduce conflict. *)
argument_expression_list:
  | x = assignment_expression { [x] }
  | x = assignment_expression; COMMA; xs = argument_expression_list { x::xs }

primary_expression:
  | i = identifier          { Ast.Expr.Identifier i }
  | k = constant            { Ast.Expr.Constant   k }
  | s = STRING              { Ast.Expr.String     s }
  | e = parened(expression) { Ast.Expr.Brackets   e }

constant:
  | i = INT_LIT   { Ast.Constant.Integer i }
  | c = CHAR_LIT  { Ast.Constant.Char    c }
  | f = FLOAT_LIT { Ast.Constant.Float   f }

identifier:
  | i = IDENTIFIER { i }
(* Contextual keywords. *)
  | LIT_EXISTS { "exists" }
