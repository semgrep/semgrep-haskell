(**
   Boilerplate to be used as a template when mapping the haskell CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_comma (env : env) (tok : CST.comma) =
  (* comma *) token env tok

let map_namespace (env : env) (x : CST.namespace) =
  (match x with
  | `Pat tok -> R.Case ("Pat",
      (* "pattern" *) token env tok
    )
  | `Type tok -> R.Case ("Type",
      (* "type" *) token env tok
    )
  )

let map_where (env : env) (tok : CST.where) =
  (* where *) token env tok

let map_type_role (env : env) (x : CST.type_role) =
  (match x with
  | `Repr tok -> R.Case ("Repr",
      (* "representational" *) token env tok
    )
  | `Nomi tok -> R.Case ("Nomi",
      (* "nominal" *) token env tok
    )
  | `Phan tok -> R.Case ("Phan",
      (* "phantom" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let map_do_keyword (env : env) (x : CST.do_keyword) =
  (match x with
  | `Mdo tok -> R.Case ("Mdo",
      (* "mdo" *) token env tok
    )
  | `Do tok -> R.Case ("Do",
      (* "do" *) token env tok
    )
  )

let map_layout_start (env : env) (tok : CST.layout_start) =
  (* layout_start *) token env tok

let map_splice_dollar (env : env) (tok : CST.splice_dollar) =
  (* splice_dollar *) token env tok

let map_quasiquote_start (env : env) (tok : CST.quasiquote_start) =
  (* quasiquote_start *) token env tok

let map_arrow (env : env) (x : CST.arrow) =
  (match x with
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\134\146" *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  )

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_con_unit (env : env) ((v1, v2) : CST.con_unit) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* ")" *) token env v2 in
  R.Tuple [v1; v2]

let map_forall_kw (env : env) (x : CST.forall_kw) =
  (match x with
  | `Forall tok -> R.Case ("Forall",
      (* "forall" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\136\128" *) token env tok
    )
  )

let map_conid (env : env) (tok : CST.conid) =
  (* pattern "[\\p{Lu}\\p{Lt}](\\w|')*#?" *) token env tok

let map_con_list (env : env) ((v1, v2) : CST.con_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = (* "]" *) token env v2 in
  R.Tuple [v1; v2]

let map_type_star (env : env) (x : CST.type_star) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\152\133" *) token env tok
    )
  )

let map_imm_tok_at (env : env) (tok : CST.imm_tok_at) =
  (* "@" *) token env tok

let map_layout_semicolon (env : env) (tok : CST.layout_semicolon) =
  (* layout_semicolon *) token env tok

let map_larrow (env : env) (x : CST.larrow) =
  (match x with
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\134\144" *) token env tok
    )
  | `LTDASH tok -> R.Case ("LTDASH",
      (* "<-" *) token env tok
    )
  )

let map_carrow (env : env) (x : CST.carrow) =
  (match x with
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\135\146" *) token env tok
    )
  | `EQGT tok -> R.Case ("EQGT",
      (* "=>" *) token env tok
    )
  )

let map_binary_literal (env : env) (tok : CST.binary_literal) =
  (* binary_literal *) token env tok

let map_char (env : env) (tok : CST.char) =
  (* char *) token env tok

let map_unboxed_close (env : env) (tok : CST.unboxed_close) =
  (* unboxed_close *) token env tok

let map_colon2 (env : env) (x : CST.colon2) =
  (match x with
  | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
      (* "\226\136\183" *) token env tok
    )
  | `COLONCOLON tok -> R.Case ("COLONCOLON",
      (* "::" *) token env tok
    )
  )

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_deriving_strategy (env : env) (x : CST.deriving_strategy) =
  (match x with
  | `Stock tok -> R.Case ("Stock",
      (* "stock" *) token env tok
    )
  | `Newt tok -> R.Case ("Newt",
      (* "newtype" *) token env tok
    )
  | `Anyc tok -> R.Case ("Anyc",
      (* "anyclass" *) token env tok
    )
  )

let map_unboxed_open (env : env) (x : CST.unboxed_open) =
  (match x with
  | `LPARHASHSPACE tok -> R.Case ("LPARHASHSPACE",
      (* "(# " *) token env tok
    )
  | `LPARHASHLF tok -> R.Case ("LPARHASHLF",
      (* "(#\n" *) token env tok
    )
  )

let map_quasiquote_bar (env : env) (tok : CST.quasiquote_bar) =
  (* quasiquote_bar *) token env tok

let map_empty_file (env : env) (tok : CST.empty_file) =
  (* empty_file *) token env tok

let map_safety (env : env) (x : CST.safety) =
  (match x with
  | `Unsafe tok -> R.Case ("Unsafe",
      (* "unsafe" *) token env tok
    )
  | `Safe tok -> R.Case ("Safe",
      (* "safe" *) token env tok
    )
  | `Inte tok -> R.Case ("Inte",
      (* "interruptible" *) token env tok
    )
  )

let map_varid (env : env) (tok : CST.varid) =
  (* pattern "[_\\p{Ll}](\\w|')*#?" *) token env tok

let map_quasiquote_body (env : env) (tok : CST.quasiquote_body) =
  (* quasiquote_body *) token env tok

let map_implicit_parid (env : env) (tok : CST.implicit_parid) =
  (* pattern "\\?[_\\p{Ll}](\\w|')*" *) token env tok

let map_dot (env : env) (tok : CST.dot) =
  (* dot *) token env tok

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_octal_literal (env : env) (tok : CST.octal_literal) =
  (* octal_literal *) token env tok

let map_strict (env : env) (tok : CST.strict) =
  (* strict *) token env tok

let map_label (env : env) (tok : CST.label) =
  (* pattern "#[_\\p{Ll}](\\w|')*" *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_layout_end (env : env) (tok : CST.layout_end) =
  (* layout_end *) token env tok

let map_tok_barrbrack (env : env) (tok : CST.tok_barrbrack) =
  (* tok_barrbrack *) token env tok

let map_varsym (env : env) (tok : CST.varsym) =
  (* varsym *) token env tok

let map_calling_convention (env : env) (x : CST.calling_convention) =
  (match x with
  | `Ccall tok -> R.Case ("Ccall",
      (* "ccall" *) token env tok
    )
  | `Stdc tok -> R.Case ("Stdc",
      (* "stdcall" *) token env tok
    )
  | `Cpluss tok -> R.Case ("Cpluss",
      (* "cplusplus" *) token env tok
    )
  | `Jvm tok -> R.Case ("Jvm",
      (* "jvm" *) token env tok
    )
  | `Dotnet tok -> R.Case ("Dotnet",
      (* "dotnet" *) token env tok
    )
  | `Prim tok -> R.Case ("Prim",
      (* "prim" *) token env tok
    )
  | `Capi tok -> R.Case ("Capi",
      (* "capi" *) token env tok
    )
  )

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* hex_literal *) token env tok

let map_consym (env : env) (tok : CST.consym) =
  (* consym *) token env tok

let map_tyconsym (env : env) (tok : CST.tyconsym) =
  (* tyconsym *) token env tok

let map_con_tuple (env : env) ((v1, v2, v3) : CST.con_tuple) =
  let v1 = (* "(" *) token env v1 in
  let v2 = R.List (List.map (token env (* comma *)) v2) in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_forall_dot (env : env) (x : CST.forall_dot) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `Arrow x -> R.Case ("Arrow",
      map_arrow env x
    )
  )

let map_anon_choice_SEMI_ab17175 (env : env) (x : CST.anon_choice_SEMI_ab17175) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Layout_semi tok -> R.Case ("Layout_semi",
      (* layout_semicolon *) token env tok
    )
  )

let map_constructor (env : env) (x : CST.constructor) =
  (match x with
  | `Conid tok -> R.Case ("Conid",
      (* pattern "[\\p{Lu}\\p{Lt}](\\w|')*#?" *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_foreign_pre (env : env) ((v1, v2) : CST.foreign_pre) =
  let v1 = map_calling_convention env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_safety env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_integer (env : env) (x : CST.integer) =
  (match x with
  | `Bin_lit tok -> R.Case ("Bin_lit",
      (* binary_literal *) token env tok
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Octal_lit tok -> R.Case ("Octal_lit",
      (* octal_literal *) token env tok
    )
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* hex_literal *) token env tok
    )
  )

let map_gcon_literal (env : env) (x : CST.gcon_literal) =
  (match x with
  | `Con_unit x -> R.Case ("Con_unit",
      map_con_unit env x
    )
  | `Con_list x -> R.Case ("Con_list",
      map_con_list env x
    )
  | `Con_tuple x -> R.Case ("Con_tuple",
      map_con_tuple env x
    )
  )

let map_tyfam_injectivity (env : env) ((v1, v2, v3, v4) : CST.tyfam_injectivity) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (* pattern "[_\\p{Ll}](\\w|')*#?" *) token env v2
  in
  let v3 = map_arrow env v3 in
  let v4 =
    R.List (List.map (token env (* pattern "[_\\p{Ll}](\\w|')*#?" *)) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Varid tok -> R.Case ("Varid",
      (* pattern "[_\\p{Ll}](\\w|')*#?" *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_fundep (env : env) ((v1, v2, v3) : CST.fundep) =
  let v1 =
    R.List (List.map (token env (* pattern "[_\\p{Ll}](\\w|')*#?" *)) v1)
  in
  let v2 = map_arrow env v2 in
  let v3 =
    R.List (List.map (token env (* pattern "[_\\p{Ll}](\\w|')*#?" *)) v3)
  in
  R.Tuple [v1; v2; v3]

let map_stringly (env : env) (x : CST.stringly) =
  (match x with
  | `Str tok -> R.Case ("Str",
      (* string *) token env tok
    )
  | `Char tok -> R.Case ("Char",
      (* char *) token env tok
    )
  )

let map_modid (env : env) (x : CST.modid) =
  map_constructor env x

let map_tyconid (env : env) (x : CST.tyconid) =
  map_constructor env x

let map_operator_minus (env : env) (x : CST.operator_minus) =
  (match x with
  | `Op tok -> R.Case ("Op",
      (* varsym *) token env tok
    )
  | `Minus tok -> R.Case ("Minus",
      (* "-" *) token env tok
    )
  )

let map_number (env : env) (x : CST.number) =
  (match x with
  | `Int x -> R.Case ("Int",
      map_integer env x
    )
  | `Float tok -> R.Case ("Float",
      (* float *) token env tok
    )
  )

let map_type_operator (env : env) (x : CST.type_operator) =
  (match x with
  | `Tyco tok -> R.Case ("Tyco",
      (* tyconsym *) token env tok
    )
  | `Cons_op tok -> R.Case ("Cons_op",
      (* consym *) token env tok
    )
  )

let map_fundeps (env : env) ((v1, v2, v3) : CST.fundeps) =
  let v1 = (* "|" *) token env v1 in
  let v2 = map_fundep env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_fundep env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_qualifying_module (env : env) (xs : CST.qualifying_module) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = map_modid env v1 in
    let v2 = (* dot *) token env v2 in
    R.Tuple [v1; v2]
  ) xs)

let map_ticked_tycon (env : env) ((v1, v2, v3) : CST.ticked_tycon) =
  let v1 = (* "`" *) token env v1 in
  let v2 = map_tyconid env v2 in
  let v3 = (* "`" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_con (env : env) (x : CST.con) =
  (match x with
  | `Cons x -> R.Case ("Cons",
      map_tyconid env x
    )
  | `LPAR_cons_op_RPAR (v1, v2, v3) -> R.Case ("LPAR_cons_op_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* consym *) token env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_varop (env : env) (x : CST.varop) =
  (match x with
  | `Choice_op x -> R.Case ("Choice_op",
      map_operator_minus env x
    )
  | `BQUOT_var_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_var_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_variable env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_pat_name (env : env) (x : CST.pat_name) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `LPAR_choice_op_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_op_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_operator_minus env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Choice_str x -> R.Case ("Choice_str",
      map_stringly env x
    )
  | `Choice_int x -> R.Case ("Choice_int",
      map_number env x
    )
  )

let map_simple_tycon (env : env) (x : CST.simple_tycon) =
  (match x with
  | `Cons x -> R.Case ("Cons",
      map_tyconid env x
    )
  | `LPAR_type_op_RPAR (v1, v2, v3) -> R.Case ("LPAR_type_op_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_operator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qualified_constructor_operator (env : env) ((v1, v2) : CST.qualified_constructor_operator) =
  let v1 = map_qualifying_module env v1 in
  let v2 = (* consym *) token env v2 in
  R.Tuple [v1; v2]

let map_do_module (env : env) ((v1, v2) : CST.do_module) =
  let v1 = map_qualifying_module env v1 in
  let v2 = map_do_keyword env v2 in
  R.Tuple [v1; v2]

let map_qualified_operator (env : env) ((v1, v2) : CST.qualified_operator) =
  let v1 = map_qualifying_module env v1 in
  let v2 = map_operator_minus env v2 in
  R.Tuple [v1; v2]

let map_conop (env : env) (x : CST.conop) =
  (match x with
  | `Cons_op tok -> R.Case ("Cons_op",
      (* consym *) token env tok
    )
  | `BQUOT_cons_BQUOT x -> R.Case ("BQUOT_cons_BQUOT",
      map_ticked_tycon env x
    )
  )

let map_simple_tyconop (env : env) (x : CST.simple_tyconop) =
  (match x with
  | `Ticked_tycon x -> R.Case ("Ticked_tycon",
      map_ticked_tycon env x
    )
  | `Type_op x -> R.Case ("Type_op",
      map_type_operator env x
    )
  )

let map_name (env : env) (x : CST.name) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_pat_name env x
    )
  | `Choice_cons x -> R.Case ("Choice_cons",
      map_con env x
    )
  )

let map_import_name (env : env) (x : CST.import_name) =
  (match x with
  | `Choice_cons x -> R.Case ("Choice_cons",
      map_con env x
    )
  | `Choice_var x -> R.Case ("Choice_var",
      map_pat_name env x
    )
  )

let map_fun_name (env : env) (x : CST.fun_name) =
  (match x with
  | `Choice_var x -> R.Case ("Choice_var",
      map_pat_name env x
    )
  | `Impl_parid tok -> R.Case ("Impl_parid",
      (* pattern "\\?[_\\p{Ll}](\\w|')*" *) token env tok
    )
  )

let map_literal_ (env : env) (x : CST.literal_) =
  (match x with
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Choice_con_unit x -> R.Case ("Choice_con_unit",
      map_gcon_literal env x
    )
  )

let map_type_literal (env : env) (x : CST.type_literal) =
  (match x with
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Con_unit x -> R.Case ("Con_unit",
      map_con_unit env x
    )
  | `Con_list x -> R.Case ("Con_list",
      map_con_list env x
    )
  | `Con_tuple x -> R.Case ("Con_tuple",
      map_con_tuple env x
    )
  )

let map_qmodid (env : env) (x : CST.qmodid) =
  (match x with
  | `Qual_module (v1, v2) -> R.Case ("Qual_module",
      let v1 = map_qualifying_module env v1 in
      let v2 = map_modid env v2 in
      R.Tuple [v1; v2]
    )
  | `Modid x -> R.Case ("Modid",
      map_modid env x
    )
  )

let map_qconsym (env : env) (x : CST.qconsym) =
  (match x with
  | `Qual_cons_op x -> R.Case ("Qual_cons_op",
      map_qualified_constructor_operator env x
    )
  | `Cons_op tok -> R.Case ("Cons_op",
      (* consym *) token env tok
    )
  )

let map_qvarid (env : env) (x : CST.qvarid) =
  (match x with
  | `Qual_var (v1, v2) -> R.Case ("Qual_var",
      let v1 = map_qualifying_module env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    )
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  )

let map_qtyconid (env : env) (x : CST.qtyconid) =
  (match x with
  | `Qual_type (v1, v2) -> R.Case ("Qual_type",
      let v1 = map_qualifying_module env v1 in
      let v2 = map_tyconid env v2 in
      R.Tuple [v1; v2]
    )
  | `Cons x -> R.Case ("Cons",
      map_tyconid env x
    )
  )

let map_qvarsym (env : env) (x : CST.qvarsym) =
  (match x with
  | `Qual_op x -> R.Case ("Qual_op",
      map_qualified_operator env x
    )
  | `Choice_op x -> R.Case ("Choice_op",
      map_operator_minus env x
    )
  )

let map_qvarsym_nominus (env : env) (x : CST.qvarsym_nominus) =
  (match x with
  | `Qual_op x -> R.Case ("Qual_op",
      map_qualified_operator env x
    )
  | `Op tok -> R.Case ("Op",
      (* varsym *) token env tok
    )
  )

let map_qualified_type_operator_ (env : env) (x : CST.qualified_type_operator_) =
  (match x with
  | `Qual_type_op (v1, v2) -> R.Case ("Qual_type_op",
      let v1 = map_qualifying_module env v1 in
      let v2 = (* tyconsym *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Qual_cons_op x -> R.Case ("Qual_cons_op",
      map_qualified_constructor_operator env x
    )
  )

let map_qconid (env : env) (x : CST.qconid) =
  (match x with
  | `Qual_cons (v1, v2) -> R.Case ("Qual_cons",
      let v1 = map_qualifying_module env v1 in
      let v2 = map_tyconid env v2 in
      R.Tuple [v1; v2]
    )
  | `Cons x -> R.Case ("Cons",
      map_tyconid env x
    )
  )

let map_op (env : env) (x : CST.op) =
  (match x with
  | `Varop x -> R.Case ("Varop",
      map_varop env x
    )
  | `Choice_cons_op x -> R.Case ("Choice_cons_op",
      map_conop env x
    )
  )

let map_export_names (env : env) ((v1, v2, v3) : CST.export_names) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        | `Opt_name_rep_comma_name opt -> R.Case ("Opt_name_rep_comma_name",
            (match opt with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_name env v1 in
                let v2 =
                  R.List (List.map (fun (v1, v2) ->
                    let v1 = (* comma *) token env v1 in
                    let v2 = map_name env v2 in
                    R.Tuple [v1; v2]
                  ) v2)
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_import_con_names (env : env) ((v1, v2, v3) : CST.import_con_names) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `DOTDOT tok -> R.Case ("DOTDOT",
            (* ".." *) token env tok
          )
        | `Import_name_rep_comma_import_name (v1, v2) -> R.Case ("Import_name_rep_comma_import_name",
            let v1 = map_import_name env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* comma *) token env v1 in
                let v2 = map_import_name env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_quasiquote (env : env) ((v1, v2, v3) : CST.quasiquote) =
  let v1 =
    (match v1 with
    | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
        (* "\226\159\166" *) token env tok
      )
    | `Quas_start_opt_choice_qual_var_quas_bar (v1, v2, v3) -> R.Case ("Quas_start_opt_choice_qual_var_quas_bar",
        let v1 = (* quasiquote_start *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_qvarid env x
            ))
          | None -> R.Option None)
        in
        let v3 = (* quasiquote_bar *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* quasiquote_body *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Tok_barr x -> R.Case ("Tok_barr",
        map_tok_barrbrack env x
      )
    | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
        (* "\226\159\167" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3]

let map_qvarop (env : env) (x : CST.qvarop) =
  (match x with
  | `Choice_qual_op x -> R.Case ("Choice_qual_op",
      map_qvarsym env x
    )
  | `BQUOT_choice_qual_var_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_choice_qual_var_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qvarid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qvar (env : env) (x : CST.qvar) =
  (match x with
  | `Choice_qual_var x -> R.Case ("Choice_qual_var",
      map_qvarid env x
    )
  | `LPAR_choice_qual_op_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_qual_op_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_qvarsym env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qvarop_nominus (env : env) (x : CST.qvarop_nominus) =
  (match x with
  | `Choice_qual_op x -> R.Case ("Choice_qual_op",
      map_qvarsym_nominus env x
    )
  | `BQUOT_choice_qual_var_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_choice_qual_var_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qvarid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qtyconsym (env : env) (x : CST.qtyconsym) =
  (match x with
  | `Qual_type_op_ x -> R.Case ("Qual_type_op_",
      map_qualified_type_operator_ env x
    )
  | `Type_op x -> R.Case ("Type_op",
      map_type_operator env x
    )
  )

let map_qconop (env : env) (x : CST.qconop) =
  (match x with
  | `Choice_qual_cons_op x -> R.Case ("Choice_qual_cons_op",
      map_qconsym env x
    )
  | `BQUOT_choice_qual_cons_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_choice_qual_cons_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qconid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qcon (env : env) (x : CST.qcon) =
  (match x with
  | `Choice_qual_cons x -> R.Case ("Choice_qual_cons",
      map_qconid env x
    )
  | `LPAR_choice_qual_cons_op_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_qual_cons_op_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_qconsym env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_import_item (env : env) ((v1, v2) : CST.import_item) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_namespace env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Choice_var x -> R.Case ("Choice_var",
        map_pat_name env x
      )
    | `Choice_cons_opt_import_con_names (v1, v2) -> R.Case ("Choice_cons_opt_import_con_names",
        let v1 = map_simple_tycon env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_import_con_names env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2]

let map_qtycon (env : env) (x : CST.qtycon) =
  (match x with
  | `Choice_qual_type x -> R.Case ("Choice_qual_type",
      map_qtyconid env x
    )
  | `LPAR_choice_qual_type_op__RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_qual_type_op__RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_qtyconsym env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qtyconops (env : env) (x : CST.qtyconops) =
  (match x with
  | `Ticked_qtycon (v1, v2, v3) -> R.Case ("Ticked_qtycon",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_qtyconid env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_qual_type_op_ x -> R.Case ("Choice_qual_type_op_",
      map_qtyconsym env x
    )
  )

let map_qop_nominus (env : env) (x : CST.qop_nominus) =
  (match x with
  | `Qvarop_nominus x -> R.Case ("Qvarop_nominus",
      map_qvarop_nominus env x
    )
  | `Choice_choice_qual_cons_op x -> R.Case ("Choice_choice_qual_cons_op",
      map_qconop env x
    )
  )

let map_qop (env : env) (x : CST.qop) =
  (match x with
  | `Qvarop x -> R.Case ("Qvarop",
      map_qvarop env x
    )
  | `Choice_choice_qual_cons_op x -> R.Case ("Choice_choice_qual_cons_op",
      map_qconop env x
    )
  )

let map_pat_constructor (env : env) (x : CST.pat_constructor) =
  map_qcon env x

let map_import_list (env : env) ((v1, v2, v3, v4) : CST.import_list) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "hiding" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_import_item env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* comma *) token env v1 in
            let v2 = map_import_item env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* comma *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_export (env : env) (x : CST.export) =
  (match x with
  | `Choice_choice_qual_var x -> R.Case ("Choice_choice_qual_var",
      map_qvar env x
    )
  | `Opt_name_choice_choice_qual_type_opt_export_names (v1, v2, v3) -> R.Case ("Opt_name_choice_choice_qual_type_opt_export_names",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_namespace env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_qtycon env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_export_names env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Module_qmodid (v1, v2) -> R.Case ("Module_qmodid",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_qmodid env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_qname (env : env) (x : CST.qname) =
  (match x with
  | `Choice_choice_qual_var x -> R.Case ("Choice_choice_qual_var",
      map_qvar env x
    )
  | `Qcon x -> R.Case ("Qcon",
      map_pat_constructor env x
    )
  )

let map_exp_name (env : env) (x : CST.exp_name) =
  (match x with
  | `Choice_choice_qual_var x -> R.Case ("Choice_choice_qual_var",
      map_qvar env x
    )
  | `Qcon x -> R.Case ("Qcon",
      map_pat_constructor env x
    )
  | `Impl_parid tok -> R.Case ("Impl_parid",
      (* pattern "\\?[_\\p{Ll}](\\w|')*" *) token env tok
    )
  | `Label tok -> R.Case ("Label",
      (* pattern "#[_\\p{Ll}](\\w|')*" *) token env tok
    )
  )

let map_exports (env : env) ((v1, v2, v3, v4) : CST.exports) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_export env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* comma *) token env v1 in
            let v2 = map_export env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* comma *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_gtycon (env : env) (x : CST.gtycon) =
  (match x with
  | `Prom_tycon (v1, v2) -> R.Case ("Prom_tycon",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_qtycon env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_qual_type x -> R.Case ("Choice_choice_qual_type",
      map_qtycon env x
    )
  | `Tycon_arrow (v1, v2, v3) -> R.Case ("Tycon_arrow",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_arrow env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_qtyconop (env : env) (x : CST.qtyconop) =
  (match x with
  | `Prom_tyco (v1, v2) -> R.Case ("Prom_tyco",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_qtyconops env v2 in
      R.Tuple [v1; v2]
    )
  | `Qtycos x -> R.Case ("Qtycos",
      map_qtyconops env x
    )
  )

let rec map_aexp (env : env) (x : CST.aexp) =
  (match x with
  | `Exp_name x -> R.Case ("Exp_name",
      map_exp_name env x
    )
  | `Exp_parens x -> R.Case ("Exp_parens",
      map_exp_parens env x
    )
  | `Exp_tuple_ (v1, v2, v3) -> R.Case ("Exp_tuple_",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_exp_tuple env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_list (v1, v2, v3, v4) -> R.Case ("Exp_list",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_exp env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_th_quoted_name x -> R.Case ("Exp_th_quoted_name",
      map_exp_th_quoted_name env x
    )
  | `Exp_type_app (v1, v2) -> R.Case ("Exp_type_app",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_atype env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_lambda_case (v1, v2, v3) -> R.Case ("Exp_lambda_case",
      let v1 = (* "\\" *) token env v1 in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_alts env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_do (v1, v2) -> R.Case ("Exp_do",
      let v1 =
        (match v1 with
        | `Do_module x -> R.Case ("Do_module",
            map_do_module env x
          )
        | `Do_kw x -> R.Case ("Do_kw",
            map_do_keyword env x
          )
        )
      in
      let v2 =
        map_anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc env v2
      in
      R.Tuple [v1; v2]
    )
  | `Exp_record (v1, v2, v3, v4, v5) -> R.Case ("Exp_record",
      let v1 = map_aexp env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_exp_field env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_exp_field env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exp_arit_seq (v1, v2, v3, v4, v5, v6) -> R.Case ("Exp_arit_seq",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* comma *) token env v1 in
            let v2 = map_exp env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ".." *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_exp env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exp_list_comp (v1, v2, v3, v4, v5, v6) -> R.Case ("Exp_list_comp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* "|" *) token env v3 in
      let v4 = map_qual env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_qual env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exp_sect_left (v1, v2, v3, v4) -> R.Case ("Exp_sect_left",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_top_splice env v2 in
      let v3 = map_qop env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_sect_right (v1, v2, v3, v4) -> R.Case ("Exp_sect_right",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_qop_nominus env v2 in
      let v3 = map_top_splice env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_unbo_tuple (v1, v2, v3) -> R.Case ("Exp_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_exp env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_anon_rep_comma_opt_exp_fc8072d env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_unbo_sum_ (v1, v2, v3) -> R.Case ("Exp_unbo_sum_",
      let v1 = map_unboxed_open env v1 in
      let v2 = map_exp_unboxed_sum env v2 in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Splice x -> R.Case ("Splice",
      map_splice env x
    )
  | `Quas x -> R.Case ("Quas",
      map_quasiquote env x
    )
  | `Lit_ x -> R.Case ("Lit_",
      map_literal_ env x
    )
  )

and map_alt (env : env) ((v1, v2, v3) : CST.alt) =
  let v1 = map_pat env v1 in
  let v2 = map_alt_variants env v2 in
  let v3 = map_anon_opt_where_opt_decls_4a349ec env v3 in
  R.Tuple [v1; v2; v3]

and map_alt_variants (env : env) (x : CST.alt_variants) =
  (match x with
  | `Arrow_exp (v1, v2) -> R.Case ("Arrow_exp",
      let v1 = map_arrow env v1 in
      let v2 = map_exp env v2 in
      R.Tuple [v1; v2]
    )
  | `Rep1_gdpat xs -> R.Case ("Rep1_gdpat",
      R.List (List.map (map_gdpat env) xs)
    )
  )

and map_alts (env : env) (x : CST.alts) =
  (match x with
  | `LCURL_opt_alt_rep_SEMI_alt_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_alt_rep_SEMI_alt_opt_SEMI_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_alt env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* ";" *) token env v1 in
                let v2 = map_alt env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Layout_start_opt_alt_rep_choice_SEMI_alt_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_alt_rep_choice_SEMI_alt_opt_choice_SEMI_layout_end",
      let v1 = (* layout_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_alt env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                let v2 = map_alt env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_anon_choice_SEMI_ab17175 env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v3 = (* layout_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL_2255345 (env : env) ((v1, v2, v3, v4) : CST.anon_LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL_2255345) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_decl_rep_SEMI_decl_5323e44 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc (env : env) (x : CST.anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc) =
  (match x with
  | `LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_stmt_rep_SEMI_stmt_7a63907 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Layout_start_opt_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_layout_end",
      let v1 = (* layout_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_a6f3eb1 env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* layout_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_cons__a70bc96 (env : env) (x : CST.anon_choice_cons__a70bc96) =
  (match x with
  | `Cons_ x -> R.Case ("Cons_",
      map_constraint__ env x
    )
  | `Impl_param x -> R.Case ("Impl_param",
      map_implicit_param env x
    )
  )

and map_anon_decl_rep_SEMI_decl_5323e44 (env : env) ((v1, v2) : CST.anon_decl_rep_SEMI_decl_5323e44) =
  let v1 = map_decl env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_decl env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_decl_rep_choice_SEMI_decl_opt_choice_SEMI_896561c (env : env) ((v1, v2, v3) : CST.anon_decl_rep_choice_SEMI_decl_opt_choice_SEMI_896561c) =
  let v1 = map_decl env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_SEMI_ab17175 env v1 in
      let v2 = map_decl env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_SEMI_ab17175 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_opt_where_opt_decls_4a349ec (env : env) (opt : CST.anon_opt_where_opt_decls_4a349ec) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 = (* where *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_decls env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_rep_comma_opt_exp_fc8072d (env : env) (xs : CST.anon_rep_comma_opt_exp_fc8072d) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* comma *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_exp env x
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_anon_stmt_rep_SEMI_stmt_7a63907 (env : env) ((v1, v2) : CST.anon_stmt_rep_SEMI_stmt_7a63907) =
  let v1 = map_stmt env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_stmt env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_a6f3eb1 (env : env) ((v1, v2, v3) : CST.anon_stmt_rep_choice_SEMI_stmt_opt_choice_SEMI_a6f3eb1) =
  let v1 = map_stmt env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_SEMI_ab17175 env v1 in
      let v2 = map_stmt env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_SEMI_ab17175 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_apat (env : env) (x : CST.apat) =
  (match x with
  | `Pat_name x -> R.Case ("Pat_name",
      map_pat_name env x
    )
  | `Pat_as (v1, v2, v3) -> R.Case ("Pat_as",
      let v1 = map_variable env v1 in
      let v2 = map_imm_tok_at env v2 in
      let v3 = map_apat env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_cons x -> R.Case ("Pat_cons",
      map_pat_constructor env x
    )
  | `Pat_record (v1, v2) -> R.Case ("Pat_record",
      let v1 = map_pat_constructor env v1 in
      let v2 = map_pat_fields env v2 in
      R.Tuple [v1; v2]
    )
  | `Lit_ x -> R.Case ("Lit_",
      map_literal_ env x
    )
  | `Pat_wild tok -> R.Case ("Pat_wild",
      (* "_" *) token env tok
    )
  | `Pat_parens (v1, v2, v3) -> R.Case ("Pat_parens",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_nested_pat env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_tuple (v1, v2, v3, v4) -> R.Case ("Pat_tuple",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_nested_pat env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_nested_pat env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_unbo_tuple (v1, v2, v3) -> R.Case ("Pat_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_nested_pat env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* comma *) token env v1 in
                let v2 = map_nested_pat env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_unbo_sum_ (v1, v2, v3) -> R.Case ("Pat_unbo_sum_",
      let v1 = map_unboxed_open env v1 in
      let v2 = map_pat_unboxed_sum env v2 in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pat_list (v1, v2, v3, v4) -> R.Case ("Pat_list",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_nested_pat env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_nested_pat env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pat_strict (v1, v2) -> R.Case ("Pat_strict",
      let v1 = (* strict *) token env v1 in
      let v2 = map_apat env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_irre (v1, v2) -> R.Case ("Pat_irre",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_apat env v2 in
      R.Tuple [v1; v2]
    )
  | `Splice x -> R.Case ("Splice",
      map_splice env x
    )
  | `Quas x -> R.Case ("Quas",
      map_quasiquote env x
    )
  )

and map_atype (env : env) (x : CST.atype) =
  (match x with
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Type_star x -> R.Case ("Type_star",
      map_type_star env x
    )
  | `Type_lit_ x -> R.Case ("Type_lit_",
      map_type_literal_ env x
    )
  | `Type_parens (v1, v2, v3) -> R.Case ("Type_parens",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_or_implicit env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_unbo_tuple (v1, v2, v3) -> R.Case ("Type_unbo_tuple",
      let v1 = map_unboxed_open env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_type_or_implicit env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* comma *) token env v1 in
                let v2 = map_type_or_implicit env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_unbo_sum (v1, v2, v3) -> R.Case ("Type_unbo_sum",
      let v1 = map_unboxed_open env v1 in
      let v2 = map_type_sum env v2 in
      let v3 = (* unboxed_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Splice x -> R.Case ("Splice",
      map_splice env x
    )
  | `Quas x -> R.Case ("Quas",
      map_quasiquote env x
    )
  )

and map_bind_pattern (env : env) ((v1, v2, v3) : CST.bind_pattern) =
  let v1 = map_typed_pat env v1 in
  let v2 = map_larrow env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

and map_btype (env : env) (x : CST.btype) =
  (match x with
  | `Atype x -> R.Case ("Atype",
      map_atype env x
    )
  | `Type_apply (v1, v2) -> R.Case ("Type_apply",
      let v1 = map_atype env v1 in
      let v2 = R.List (List.map (map_atype env) v2) in
      R.Tuple [v1; v2]
    )
  )

and map_constraint_ (env : env) (x : CST.constraint_) =
  (match x with
  | `Type_name_rep_atype (v1, v2) -> R.Case ("Type_name_rep_atype",
      let v1 = map_type_name env v1 in
      let v2 = R.List (List.map (map_atype env) v2) in
      R.Tuple [v1; v2]
    )
  | `Type_infix_ x -> R.Case ("Type_infix_",
      map_type_infix_ env x
    )
  )

and map_constraint__ (env : env) (x : CST.constraint__) =
  (match x with
  | `Quan_cons (v1, v2, v3) -> R.Case ("Quan_cons",
      let v1 = map_forall env v1 in
      let v2 = map_forall_dot env v2 in
      let v3 = map_constraint__ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_cont (v1, v2) -> R.Case ("Cons_cont",
      let v1 = map_context_ env v1 in
      let v2 = map_constraint__ env v2 in
      R.Tuple [v1; v2]
    )
  | `LPAR_cons__RPAR (v1, v2, v3) -> R.Case ("LPAR_cons__RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_constraint__ env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons x -> R.Case ("Cons",
      map_constraint_ env x
    )
  )

and map_context (env : env) ((v1, v2) : CST.context) =
  let v1 = map_context_constraints env v1 in
  let v2 = map_carrow env v2 in
  R.Tuple [v1; v2]

and map_context_ (env : env) (x : CST.context_) =
  map_context env x

and map_context_constraints (env : env) (v1 : CST.context_constraints) =
  (match v1 with
  | `Cons x -> R.Case ("Cons",
      map_constraint_ env x
    )
  | `LPAR_opt_choice_cons__rep_comma_choice_cons__RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_choice_cons__rep_comma_choice_cons__RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anon_choice_cons__a70bc96 env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* comma *) token env v1 in
                let v2 = map_anon_choice_cons__a70bc96 env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_decl (env : env) (x : CST.decl) =
  (match x with
  | `Gend x -> R.Case ("Gend",
      map_gendecl env x
    )
  | `Decl_fun x -> R.Case ("Decl_fun",
      map_decl_fun env x
    )
  )

and map_decl_fun (env : env) (x : CST.decl_fun) =
  (match x with
  | `Func x -> R.Case ("Func",
      map_function_ env x
    )
  | `Funpat (v1, v2) -> R.Case ("Funpat",
      let v1 = map_typed_pat env v1 in
      let v2 = map_funrhs env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_decls (env : env) (x : CST.decls) =
  (match x with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL x -> R.Case ("LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL",
      map_anon_LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL_2255345 env x
    )
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI_layout_end",
      let v1 = (* layout_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_decl_rep_choice_SEMI_decl_opt_choice_SEMI_896561c env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* layout_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_exp (env : env) ((v1, v2) : CST.exp) =
  let v1 = map_top_splice env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_exp_apply (env : env) (x : CST.exp_apply) =
  (match x with
  | `Aexp x -> R.Case ("Aexp",
      map_aexp env x
    )
  | `Aexp_exp_apply (v1, v2) -> R.Case ("Aexp_exp_apply",
      let v1 = map_aexp env v1 in
      let v2 = map_exp_apply env v2 in
      R.Tuple [v1; v2]
    )
  | `Aexp_exp_lambda (v1, v2) -> R.Case ("Aexp_exp_lambda",
      let v1 = map_aexp env v1 in
      let v2 = map_exp_lambda env v2 in
      R.Tuple [v1; v2]
    )
  | `Aexp_exp_let_in (v1, v2) -> R.Case ("Aexp_exp_let_in",
      let v1 = map_aexp env v1 in
      let v2 = map_exp_let_in env v2 in
      R.Tuple [v1; v2]
    )
  | `Aexp_exp_cond (v1, v2) -> R.Case ("Aexp_exp_cond",
      let v1 = map_aexp env v1 in
      let v2 = map_exp_cond env v2 in
      R.Tuple [v1; v2]
    )
  | `Aexp_exp_case (v1, v2) -> R.Case ("Aexp_exp_case",
      let v1 = map_aexp env v1 in
      let v2 = map_exp_case env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_exp_case (env : env) ((v1, v2, v3, v4) : CST.exp_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_exp env v2 in
  let v3 = (* "of" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_alts env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_exp_cond (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.exp_cond) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_exp env v2 in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "then" *) token env v4 in
  let v5 = map_exp env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v7 = (* "else" *) token env v7 in
  let v8 = map_exp env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_exp_field (env : env) (x : CST.exp_field) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `Choice_choice_qual_var_opt_EQ_exp (v1, v2) -> R.Case ("Choice_choice_qual_var_opt_EQ_exp",
      let v1 = map_qvar env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_exp env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_exp_in (env : env) ((v1, v2) : CST.exp_in) =
  let v1 = (* "in" *) token env v1 in
  let v2 = map_exp env v2 in
  R.Tuple [v1; v2]

and map_exp_infix (env : env) (x : CST.exp_infix) =
  (match x with
  | `Exp_infix_ (v1, v2, v3) -> R.Case ("Exp_infix_",
      let v1 = map_top_splice env v1 in
      let v2 = map_qop env v2 in
      let v3 = map_lexp env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lexp x -> R.Case ("Lexp",
      map_lexp env x
    )
  )

and map_exp_lambda (env : env) ((v1, v2, v3, v4) : CST.exp_lambda) =
  let v1 = (* "\\" *) token env v1 in
  let v2 = map_fun_patterns env v2 in
  let v3 = map_arrow env v3 in
  let v4 = map_exp env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_exp_let (env : env) ((v1, v2) : CST.exp_let) =
  let v1 = (* "let" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_let_decls env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_exp_let_in (env : env) ((v1, v2) : CST.exp_let_in) =
  let v1 = map_exp_let env v1 in
  let v2 = map_exp_in env v2 in
  R.Tuple [v1; v2]

and map_exp_parens (env : env) ((v1, v2, v3) : CST.exp_parens) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_exp env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_exp_th_quoted_name (env : env) (x : CST.exp_th_quoted_name) =
  (match x with
  | `SQUOT_choice_choice_choice_qual_var (v1, v2) -> R.Case ("SQUOT_choice_choice_choice_qual_var",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_qname env v2 in
      R.Tuple [v1; v2]
    )
  | `SQUOTSQUOT_atype (v1, v2) -> R.Case ("SQUOTSQUOT_atype",
      let v1 = (* "''" *) token env v1 in
      let v2 = map_atype env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_exp_tuple (env : env) ((v1, v2) : CST.exp_tuple) =
  let v1 =
    (match v1 with
    | `Rep1_comma_exp (v1, v2) -> R.Case ("Rep1_comma_exp",
        let v1 = R.List (List.map (token env (* comma *)) v1) in
        let v2 = map_exp env v2 in
        R.Tuple [v1; v2]
      )
    | `Exp_comma_opt_exp (v1, v2, v3) -> R.Case ("Exp_comma_opt_exp",
        let v1 = map_exp env v1 in
        let v2 = (* comma *) token env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_exp env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v2 = map_anon_rep_comma_opt_exp_fc8072d env v2 in
  R.Tuple [v1; v2]

and map_exp_unboxed_sum (env : env) ((v1, v2) : CST.exp_unboxed_sum) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_exp env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_exp env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_fexp (env : env) (x : CST.fexp) =
  (match x with
  | `Aexp x -> R.Case ("Aexp",
      map_aexp env x
    )
  | `Exp_apply x -> R.Case ("Exp_apply",
      map_exp_apply env x
    )
  )

and map_forall (env : env) ((v1, v2) : CST.forall) =
  let v1 = map_forall_kw env v1 in
  let v2 = R.List (List.map (map_tyvar env) v2) in
  R.Tuple [v1; v2]

and map_fun_arrow (env : env) ((v1, v2) : CST.fun_arrow) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_arrow env v2 in
  R.Tuple [v1; v2]

and map_fun_guards (env : env) (xs : CST.fun_guards) =
  R.List (List.map (map_guard_equation env) xs)

and map_fun_patterns (env : env) (xs : CST.fun_patterns) =
  R.List (List.map (map_apat env) xs)

and map_function_ (env : env) ((v1, v2) : CST.function_) =
  let v1 = map_funlhs env v1 in
  let v2 = map_funrhs env v2 in
  R.Tuple [v1; v2]

and map_funlhs (env : env) (x : CST.funlhs) =
  (match x with
  | `Funvar (v1, v2) -> R.Case ("Funvar",
      let v1 = map_fun_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_fun_patterns env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Funpat_infix (v1, v2, v3) -> R.Case ("Funpat_infix",
      let v1 = map_pat env v1 in
      let v2 = map_varop env v2 in
      let v3 = map_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_funrhs (env : env) ((v1, v2) : CST.funrhs) =
  let v1 =
    (match v1 with
    | `EQ_exp (v1, v2) -> R.Case ("EQ_exp",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_exp env v2 in
        R.Tuple [v1; v2]
      )
    | `Fun_guards x -> R.Case ("Fun_guards",
        map_fun_guards env x
      )
    )
  in
  let v2 = map_anon_opt_where_opt_decls_4a349ec env v2 in
  R.Tuple [v1; v2]

and map_gdpat (env : env) ((v1, v2, v3) : CST.gdpat) =
  let v1 = map_guards env v1 in
  let v2 = map_arrow env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

and map_gendecl (env : env) (x : CST.gendecl) =
  (match x with
  | `Sign x -> R.Case ("Sign",
      map_signature env x
    )
  | `Fixity (v1, v2, v3, v4) -> R.Case ("Fixity",
      let v1 =
        (match v1 with
        | `Infixl tok -> R.Case ("Infixl",
            (* "infixl" *) token env tok
          )
        | `Infixr tok -> R.Case ("Infixr",
            (* "infixr" *) token env tok
          )
        | `Infix tok -> R.Case ("Infix",
            (* "infix" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_integer env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_op env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_op env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_guard (env : env) (x : CST.guard) =
  (match x with
  | `Pat_guard (v1, v2, v3) -> R.Case ("Pat_guard",
      let v1 = map_pat env v1 in
      let v2 = map_larrow env v2 in
      let v3 = map_top_splice env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let x -> R.Case ("Let",
      map_let_ env x
    )
  | `Exp_infix x -> R.Case ("Exp_infix",
      map_top_splice env x
    )
  )

and map_guard_equation (env : env) ((v1, v2, v3) : CST.guard_equation) =
  let v1 = map_guards env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_exp env v3 in
  R.Tuple [v1; v2; v3]

and map_guards (env : env) ((v1, v2, v3) : CST.guards) =
  let v1 = (* "|" *) token env v1 in
  let v2 = map_guard env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_guard env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_implicit_param (env : env) ((v1, v2) : CST.implicit_param) =
  let v1 =
    (* pattern "\\?[_\\p{Ll}](\\w|')*" *) token env v1
  in
  let v2 = map_type_annotation env v2 in
  R.Tuple [v1; v2]

and map_let_ (env : env) ((v1, v2) : CST.let_) =
  let v1 = (* "let" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_decls env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_let_decls (env : env) (x : CST.let_decls) =
  (match x with
  | `LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL x -> R.Case ("LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL",
      map_anon_LCURL_opt_decl_rep_SEMI_decl_opt_SEMI_RCURL_2255345 env x
    )
  | `Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI (v1, v2) -> R.Case ("Layout_start_opt_decl_rep_choice_SEMI_decl_opt_choice_SEMI",
      let v1 = (* layout_start *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_decl_rep_choice_SEMI_decl_opt_choice_SEMI_896561c env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_lexp (env : env) (x : CST.lexp) =
  (match x with
  | `Exp_let_in x -> R.Case ("Exp_let_in",
      map_exp_let_in env x
    )
  | `Exp_cond x -> R.Case ("Exp_cond",
      map_exp_cond env x
    )
  | `Exp_if_guard (v1, v2) -> R.Case ("Exp_if_guard",
      let v1 = (* "if" *) token env v1 in
      let v2 = R.List (List.map (map_gdpat env) v2) in
      R.Tuple [v1; v2]
    )
  | `Exp_case x -> R.Case ("Exp_case",
      map_exp_case env x
    )
  | `Exp_nega (v1, v2) -> R.Case ("Exp_nega",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_aexp env v2 in
      R.Tuple [v1; v2]
    )
  | `Fexp x -> R.Case ("Fexp",
      map_fexp env x
    )
  | `Exp_lambda x -> R.Case ("Exp_lambda",
      map_exp_lambda env x
    )
  )

and map_lpat (env : env) (x : CST.lpat) =
  (match x with
  | `Apat x -> R.Case ("Apat",
      map_apat env x
    )
  | `Pat_nega (v1, v2) -> R.Case ("Pat_nega",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_apat env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_apply (v1, v2) -> R.Case ("Pat_apply",
      let v1 = map_pat_constructor env v1 in
      let v2 = map_fun_patterns env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_modifier (env : env) ((v1, v2) : CST.modifier) =
  let v1 = (* "%" *) token env v1 in
  let v2 = map_atype env v2 in
  R.Tuple [v1; v2]

and map_nested_pat (env : env) (x : CST.nested_pat) =
  (match x with
  | `Typed_pat x -> R.Case ("Typed_pat",
      map_typed_pat env x
    )
  | `Pat_view (v1, v2, v3) -> R.Case ("Pat_view",
      let v1 = map_exp env v1 in
      let v2 = map_arrow env v2 in
      let v3 = map_nested_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pat (env : env) (x : CST.pat) =
  (match x with
  | `Pat_infix (v1, v2, v3) -> R.Case ("Pat_infix",
      let v1 = map_lpat env v1 in
      let v2 = map_qconop env v2 in
      let v3 = map_pat env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lpat x -> R.Case ("Lpat",
      map_lpat env x
    )
  )

and map_pat_field (env : env) (x : CST.pat_field) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `Choice_choice_qual_var_opt_EQ_nested_pat (v1, v2) -> R.Case ("Choice_choice_qual_var_opt_EQ_nested_pat",
      let v1 = map_qvar env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_nested_pat env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_pat_fields (env : env) ((v1, v2, v3) : CST.pat_fields) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_pat_field env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* comma *) token env v1 in
            let v2 = map_pat_field env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pat_unboxed_sum (env : env) ((v1, v2) : CST.pat_unboxed_sum) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_nested_pat env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_nested_pat env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_qual (env : env) (x : CST.qual) =
  (match x with
  | `Bind_pat x -> R.Case ("Bind_pat",
      map_bind_pattern env x
    )
  | `Let x -> R.Case ("Let",
      map_let_ env x
    )
  | `Tran x -> R.Case ("Tran",
      map_transform env x
    )
  | `Exp x -> R.Case ("Exp",
      map_exp env x
    )
  )

and map_signature (env : env) ((v1, v2, v3) : CST.signature) =
  let v1 = map_pat_name env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_pat_name env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_type_annotation env v3 in
  R.Tuple [v1; v2; v3]

and map_splice (env : env) ((v1, v2) : CST.splice) =
  let v1 = (* splice_dollar *) token env v1 in
  let v2 = map_splice_exp env v2 in
  R.Tuple [v1; v2]

and map_splice_exp (env : env) (x : CST.splice_exp) =
  (match x with
  | `Exp_name x -> R.Case ("Exp_name",
      map_exp_name env x
    )
  | `Exp_parens x -> R.Case ("Exp_parens",
      map_exp_parens env x
    )
  )

and map_stmt (env : env) (x : CST.stmt) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_exp env x
    )
  | `Bind_pat x -> R.Case ("Bind_pat",
      map_bind_pattern env x
    )
  | `Let x -> R.Case ("Let",
      map_let_ env x
    )
  | `Rec (v1, v2) -> R.Case ("Rec",
      let v1 = (* "rec" *) token env v1 in
      let v2 =
        map_anon_choice_LCURL_opt_stmt_rep_SEMI_stmt_opt_SEMI_RCURL_9605efc env v2
      in
      R.Tuple [v1; v2]
    )
  )

and map_top_splice (env : env) (x : CST.top_splice) =
  map_exp_infix env x

and map_transform (env : env) (x : CST.transform) =
  (match x with
  | `Then_exp_using_exp (v1, v2, v3, v4) -> R.Case ("Then_exp_using_exp",
      let v1 = (* "then group by" *) token env v1 in
      let v2 = map_exp env v2 in
      let v3 = (* "using" *) token env v3 in
      let v4 = map_exp env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Then_exp_9b4d8a6 (v1, v2) -> R.Case ("Then_exp_9b4d8a6",
      let v1 = (* "then group using" *) token env v1 in
      let v2 = map_exp env v2 in
      R.Tuple [v1; v2]
    )
  | `Then_exp_8bf9922 (v1, v2) -> R.Case ("Then_exp_8bf9922",
      let v1 = (* "then" *) token env v1 in
      let v2 = map_exp env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Type_quants (v1, v2, v3) -> R.Case ("Type_quants",
      let v1 = map_forall env v1 in
      let v2 = map_forall_dot env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_cont (v1, v2) -> R.Case ("Type_cont",
      let v1 = map_context_ env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Type_fun (v1, v2, v3) -> R.Case ("Type_fun",
      let v1 = map_type_infix env v1 in
      let v2 = map_fun_arrow env v2 in
      let v3 = map_type_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_infix x -> R.Case ("Type_infix",
      map_type_infix env x
    )
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = map_colon2 env v1 in
  let v2 = map_type_or_implicit env v2 in
  R.Tuple [v1; v2]

and map_type_infix (env : env) (x : CST.type_infix) =
  (match x with
  | `Type_infix_ x -> R.Case ("Type_infix_",
      map_type_infix_ env x
    )
  | `Btype x -> R.Case ("Btype",
      map_btype env x
    )
  )

and map_type_infix_ (env : env) ((v1, v2, v3) : CST.type_infix_) =
  let v1 = map_btype env v1 in
  let v2 = map_qtyconop env v2 in
  let v3 = map_type_infix env v3 in
  R.Tuple [v1; v2; v3]

and map_type_literal_ (env : env) (x : CST.type_literal_) =
  (match x with
  | `Type_prom_lit_c26b94c (v1, v2) -> R.Case ("Type_prom_lit_c26b94c",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_type_promotable_literal env v2 in
      R.Tuple [v1; v2]
    )
  | `Type_prom_lit_af79c83 x -> R.Case ("Type_prom_lit_af79c83",
      map_type_promotable_literal env x
    )
  )

and map_type_name (env : env) (x : CST.type_name) =
  (match x with
  | `Choice_anno_type_var x -> R.Case ("Choice_anno_type_var",
      map_tyvar env x
    )
  | `Choice_prom_tycon x -> R.Case ("Choice_prom_tycon",
      map_gtycon env x
    )
  )

and map_type_or_implicit (env : env) (x : CST.type_or_implicit) =
  (match x with
  | `Impl_param x -> R.Case ("Impl_param",
      map_implicit_param env x
    )
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  )

and map_type_promotable_literal (env : env) (x : CST.type_promotable_literal) =
  (match x with
  | `Type_lit x -> R.Case ("Type_lit",
      map_type_literal env x
    )
  | `Type_tuple_ (v1, v2, v3) -> R.Case ("Type_tuple_",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_tuple env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_list (v1, v2, v3, v4) -> R.Case ("Type_list",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_type_or_implicit env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* comma *) token env v1 in
          let v2 = map_type_or_implicit env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_type_sum (env : env) ((v1, v2) : CST.type_sum) =
  let v1 = map_type_or_implicit env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_type_or_implicit env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_tuple (env : env) ((v1, v2) : CST.type_tuple) =
  let v1 = map_type_or_implicit env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_type_or_implicit env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_typed_pat (env : env) (x : CST.typed_pat) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pat env x
    )
  | `Pat_typed (v1, v2) -> R.Case ("Pat_typed",
      let v1 = map_pat env v1 in
      let v2 = map_type_annotation env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_tyvar (env : env) (x : CST.tyvar) =
  (match x with
  | `Anno_type_var (v1, v2, v3, v4) -> R.Case ("Anno_type_var",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (* pattern "[_\\p{Ll}](\\w|')*#?" *) token env v2
      in
      let v3 = map_type_annotation env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_var tok -> R.Case ("Type_var",
      (* pattern "[_\\p{Ll}](\\w|')*#?" *) token env tok
    )
  )

let map_pattern_equals (env : env) ((v1, v2, v3) : CST.pattern_equals) =
  let v1 = map_pat env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_pat env v3 in
  R.Tuple [v1; v2; v3]

let map_forall_ (env : env) ((v1, v2) : CST.forall_) =
  let v1 = map_forall env v1 in
  let v2 = map_forall_dot env v2 in
  R.Tuple [v1; v2]

let map_pattern_type (env : env) ((v1, v2) : CST.pattern_type) =
  let v1 = map_con env v1 in
  let v2 = map_type_annotation env v2 in
  R.Tuple [v1; v2]

let map_tyfam_result_type (env : env) ((v1, v2) : CST.tyfam_result_type) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_tyvar env v2 in
  R.Tuple [v1; v2]

let map_via (env : env) ((v1, v2) : CST.via) =
  let v1 = (* "via" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

let map_strict_type (env : env) ((v1, v2) : CST.strict_type) =
  let v1 = (* strict *) token env v1 in
  let v2 = map_atype env v2 in
  R.Tuple [v1; v2]

let map_pattern_decl (env : env) ((v1, v2) : CST.pattern_decl) =
  let v1 = map_pat env v1 in
  let v2 = map_funrhs env v2 in
  R.Tuple [v1; v2]

let map_inst_tyinst (env : env) ((v1, v2, v3, v4, v5) : CST.inst_tyinst) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "instance" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_atype env) v3) in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_simpletype (env : env) (x : CST.simpletype) =
  (match x with
  | `LPAR_simp_RPAR (v1, v2, v3) -> R.Case ("LPAR_simp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_tyfam_head env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simp_infix (v1, v2, v3) -> R.Case ("Simp_infix",
      let v1 = map_tyvar env v1 in
      let v2 = map_simple_tyconop env v2 in
      let v3 = map_tyvar env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_cons_rep_choice_anno_type_var (v1, v2) -> R.Case ("Choice_cons_rep_choice_anno_type_var",
      let v1 = map_simple_tycon env v1 in
      let v2 = R.List (List.map (map_tyvar env) v2) in
      R.Tuple [v1; v2]
    )
  )

and map_tyfam_head (env : env) (x : CST.tyfam_head) =
  map_simpletype env x

let map_instance (env : env) ((v1, v2, v3, v4) : CST.instance) =
  let v1 = (* "instance" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_context_ env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_constraint_ env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_datainst (env : env) ((v1, v2, v3, v4) : CST.datainst) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_context_ env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_infix env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_annotation env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_tyfam_inj (env : env) ((v1, v2) : CST.tyfam_inj) =
  let v1 = map_tyfam_result_type env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_tyfam_injectivity env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_deriving (env : env) ((v1, v2, v3, v4) : CST.deriving) =
  let v1 = (* "deriving" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_deriving_strategy env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | `Choice_qual_type x -> R.Case ("Choice_qual_type",
        map_qtyconid env x
      )
    | `LPAR_opt_cons__rep_comma_cons__RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_cons__rep_comma_cons__RPAR",
        let v1 = (* "(" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_constraint__ env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* comma *) token env v1 in
                  let v2 = map_constraint__ env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_via env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_strict_type_5770e7f (env : env) (x : CST.anon_choice_strict_type_5770e7f) =
  (match x with
  | `Strict_type x -> R.Case ("Strict_type",
      map_strict_type env x
    )
  | `Type_infix x -> R.Case ("Type_infix",
      map_type_infix env x
    )
  )

let map_field (env : env) ((v1, v2, v3, v4) : CST.field) =
  let v1 = map_variable env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = map_colon2 env v3 in
  let v4 =
    (match v4 with
    | `Strict_type x -> R.Case ("Strict_type",
        map_strict_type env x
      )
    | `Type x -> R.Case ("Type",
        map_type_ env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

let map_pattern_arrow (env : env) ((v1, v2, v3, v4) : CST.pattern_arrow) =
  let v1 = map_pat env v1 in
  let v2 = map_larrow env v2 in
  let v3 = map_pat env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* where *) token env v1 in
        let v2 =
          (match v2 with
          | `LCURL_opt_pat_decl_rep_SEMI_pat_decl_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_pat_decl_rep_SEMI_pat_decl_opt_SEMI_RCURL",
              let v1 = (* "{" *) token env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = map_pattern_decl env v1 in
                    let v2 =
                      R.List (List.map (fun (v1, v2) ->
                        let v1 = (* ";" *) token env v1 in
                        let v2 = map_pattern_decl env v2 in
                        R.Tuple [v1; v2]
                      ) v2)
                    in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              let v3 =
                (match v3 with
                | Some tok -> R.Option (Some (
                    (* ";" *) token env tok
                  ))
                | None -> R.Option None)
              in
              let v4 = (* "}" *) token env v4 in
              R.Tuple [v1; v2; v3; v4]
            )
          | `Layout_start_opt_pat_decl_rep_choice_SEMI_pat_decl_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_pat_decl_rep_choice_SEMI_pat_decl_opt_choice_SEMI_layout_end",
              let v1 = (* layout_start *) token env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2, v3) -> R.Option (Some (
                    let v1 = map_pattern_decl env v1 in
                    let v2 =
                      R.List (List.map (fun (v1, v2) ->
                        let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                        let v2 = map_pattern_decl env v2 in
                        R.Tuple [v1; v2]
                      ) v2)
                    in
                    let v3 =
                      (match v3 with
                      | Some x -> R.Option (Some (
                          map_anon_choice_SEMI_ab17175 env x
                        ))
                      | None -> R.Option None)
                    in
                    R.Tuple [v1; v2; v3]
                  ))
                | None -> R.Option None)
              in
              let v3 = (* layout_end *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

let map_tyfam_pat (env : env) (x : CST.tyfam_pat) =
  (match x with
  | `Choice_qual_type_rep_atype (v1, v2) -> R.Case ("Choice_qual_type_rep_atype",
      let v1 = map_qtyconid env v1 in
      let v2 = R.List (List.map (map_atype env) v2) in
      R.Tuple [v1; v2]
    )
  | `Btype_qtyc_btype (v1, v2, v3) -> R.Case ("Btype_qtyc_btype",
      let v1 = map_btype env v1 in
      let v2 = map_qtyconop env v2 in
      let v3 = map_btype env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_decl_foreign (env : env) (x : CST.decl_foreign) =
  (match x with
  | `Decl_fore_import (v1, v2, v3, v4, v5) -> R.Case ("Decl_fore_import",
      let v1 = (* "foreign" *) token env v1 in
      let v2 = (* "import" *) token env v2 in
      let v3 = map_foreign_pre env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* string *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_signature env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Decl_fore_export (v1, v2, v3, v4, v5) -> R.Case ("Decl_fore_export",
      let v1 = (* "foreign" *) token env v1 in
      let v2 = (* "export" *) token env v2 in
      let v3 = map_foreign_pre env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* string *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = map_signature env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_context_newtype (env : env) (x : CST.context_newtype) =
  (match x with
  | `Cont__simp (v1, v2) -> R.Case ("Cont__simp",
      let v1 = map_context_ env v1 in
      let v2 = map_tyfam_head env v2 in
      R.Tuple [v1; v2]
    )
  | `Simp x -> R.Case ("Simp",
      map_tyfam_head env x
    )
  )

let rec map_gadt_sig (env : env) (x : CST.gadt_sig) =
  (match x with
  | `Gadt_fun (v1, v2, v3) -> R.Case ("Gadt_fun",
      let v1 = map_anon_choice_strict_type_5770e7f env v1 in
      let v2 = map_arrow env v2 in
      let v3 = map_gadt_sig env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_strict_type x -> R.Case ("Choice_strict_type",
      map_anon_choice_strict_type_5770e7f env x
    )
  )

let map_record_fields (env : env) ((v1, v2, v3, v4) : CST.record_fields) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_field env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* comma *) token env v1 in
      let v2 = map_field env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_record_field (env : env) ((v1, v2, v3) : CST.record_field) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_field env v2 in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_tyfam_eq (env : env) ((v1, v2, v3) : CST.tyfam_eq) =
  let v1 = map_tyfam_pat env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_type_or_implicit env v3 in
  R.Tuple [v1; v2; v3]

let map_cdecl (env : env) (x : CST.cdecl) =
  (match x with
  | `Gend x -> R.Case ("Gend",
      map_gendecl env x
    )
  | `Defa_sign (v1, v2) -> R.Case ("Defa_sign",
      let v1 = (* "default" *) token env v1 in
      let v2 = map_signature env v2 in
      R.Tuple [v1; v2]
    )
  | `Func x -> R.Case ("Func",
      map_function_ env x
    )
  | `Class_tyfam (v1, v2, v3, v4) -> R.Case ("Class_tyfam",
      let v1 = (* "type" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "family" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_tyfam_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Inst_tyinst x -> R.Case ("Inst_tyinst",
      map_inst_tyinst env x
    )
  | `Class_data (v1, v2, v3, v4) -> R.Case ("Class_data",
      let v1 = (* "data" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "family" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_tyfam_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_gadt_constr_type (env : env) ((v1, v2, v3, v4) : CST.gadt_constr_type) =
  let v1 = map_colon2 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_context_ env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Gadt_sig x -> R.Case ("Gadt_sig",
        map_gadt_sig env x
      )
    | `Record_fields_arrow_gadt_sig (v1, v2, v3) -> R.Case ("Record_fields_arrow_gadt_sig",
        let v1 = map_record_fields env v1 in
        let v2 = map_arrow env v2 in
        let v3 = map_gadt_sig env v3 in
        R.Tuple [v1; v2; v3]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

let map_newtype_constructor (env : env) ((v1, v2) : CST.newtype_constructor) =
  let v1 = map_tyconid env v1 in
  let v2 =
    (match v2 with
    | `Atype x -> R.Case ("Atype",
        map_atype env x
      )
    | `Record_field x -> R.Case ("Record_field",
        map_record_field env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_class_body (env : env) ((v1, v2) : CST.class_body) =
  let v1 = (* where *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `LCURL_opt_cdecl_rep_SEMI_cdecl_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_cdecl_rep_SEMI_cdecl_opt_SEMI_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_cdecl env v1 in
                  let v2 =
                    R.List (List.map (fun (v1, v2) ->
                      let v1 = (* ";" *) token env v1 in
                      let v2 = map_cdecl env v2 in
                      R.Tuple [v1; v2]
                    ) v2)
                  in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* ";" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v4 = (* "}" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        | `Layout_start_opt_cdecl_rep_choice_SEMI_cdecl_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_cdecl_rep_choice_SEMI_cdecl_opt_choice_SEMI_layout_end",
            let v1 = (* layout_start *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 = map_cdecl env v1 in
                  let v2 =
                    R.List (List.map (fun (v1, v2) ->
                      let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                      let v2 = map_cdecl env v2 in
                      R.Tuple [v1; v2]
                    ) v2)
                  in
                  let v3 =
                    (match v3 with
                    | Some x -> R.Option (Some (
                        map_anon_choice_SEMI_ab17175 env x
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            let v3 = (* layout_end *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_anon_choice_data_cons_3ed9ff3 (env : env) (x : CST.anon_choice_data_cons_3ed9ff3) =
  (match x with
  | `Data_cons (v1, v2) -> R.Case ("Data_cons",
      let v1 = map_tyconid env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Strict_type x -> R.Case ("Strict_type",
              map_strict_type env x
            )
          | `Atype x -> R.Case ("Atype",
              map_atype env x
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Data_cons_infix (v1, v2, v3) -> R.Case ("Data_cons_infix",
      let v1 = map_anon_choice_strict_type_5770e7f env v1 in
      let v2 = map_conop env v2 in
      let v3 = map_anon_choice_strict_type_5770e7f env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Data_cons_record (v1, v2) -> R.Case ("Data_cons_record",
      let v1 = map_tyconid env v1 in
      let v2 = map_record_fields env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_newtype (env : env) ((v1, v2, v3) : CST.newtype) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_newtype_constructor env v2 in
  let v3 = R.List (List.map (map_deriving env) v3) in
  R.Tuple [v1; v2; v3]

let map_constructors (env : env) ((v1, v2, v3, v4) : CST.constructors) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_forall_ env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_context_ env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_choice_data_cons_3ed9ff3 env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_forall_ env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_context_ env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_anon_choice_data_cons_3ed9ff3 env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_gadt_cons_603dbe0 (env : env) (x : CST.anon_choice_gadt_cons_603dbe0) =
  (match x with
  | `Gadt_cons (v1, v2) -> R.Case ("Gadt_cons",
      let v1 = map_con env v1 in
      let v2 = map_gadt_constr_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Deri x -> R.Case ("Deri",
      map_deriving env x
    )
  )

let map_adt_rhs (env : env) ((v1, v2, v3) : CST.adt_rhs) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_constructors env v2 in
  let v3 = R.List (List.map (map_deriving env) v3) in
  R.Tuple [v1; v2; v3]

let map_gadt_rhs (env : env) ((v1, v2) : CST.gadt_rhs) =
  let v1 = (* where *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `LCURL_opt_choice_gadt_cons_rep_SEMI_choice_gadt_cons_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_choice_gadt_cons_rep_SEMI_choice_gadt_cons_opt_SEMI_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_anon_choice_gadt_cons_603dbe0 env v1 in
                  let v2 =
                    R.List (List.map (fun (v1, v2) ->
                      let v1 = (* ";" *) token env v1 in
                      let v2 = map_anon_choice_gadt_cons_603dbe0 env v2 in
                      R.Tuple [v1; v2]
                    ) v2)
                  in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some tok -> R.Option (Some (
                  (* ";" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v4 = (* "}" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        | `Layout_start_opt_choice_gadt_cons_rep_choice_SEMI_choice_gadt_cons_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_choice_gadt_cons_rep_choice_SEMI_choice_gadt_cons_opt_choice_SEMI_layout_end",
            let v1 = (* layout_start *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 = map_anon_choice_gadt_cons_603dbe0 env v1 in
                  let v2 =
                    R.List (List.map (fun (v1, v2) ->
                      let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                      let v2 = map_anon_choice_gadt_cons_603dbe0 env v2 in
                      R.Tuple [v1; v2]
                    ) v2)
                  in
                  let v3 =
                    (match v3 with
                    | Some x -> R.Option (Some (
                        map_anon_choice_SEMI_ab17175 env x
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            let v3 = (* layout_end *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_adt (env : env) (v1 : CST.adt) =
  (match v1 with
  | `Adt_rhs x -> R.Case ("Adt_rhs",
      map_adt_rhs env x
    )
  | `Gadt_rhs x -> R.Case ("Gadt_rhs",
      map_gadt_rhs env x
    )
  )

let map_inst_datainst (env : env) (x : CST.inst_datainst) =
  (match x with
  | `Data_opt_inst_data_opt_adt (v1, v2, v3, v4) -> R.Case ("Data_opt_inst_data_opt_adt",
      let v1 = (* "data" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "instance" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_datainst env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_adt env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Newt_opt_inst_data_newt (v1, v2, v3, v4) -> R.Case ("Newt_opt_inst_data_newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "instance" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_datainst env v3 in
      let v4 = map_newtype env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_decl_datainst (env : env) (x : CST.decl_datainst) =
  (match x with
  | `Data_inst_data_opt_adt (v1, v2, v3, v4) -> R.Case ("Data_inst_data_opt_adt",
      let v1 = (* "data" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = map_datainst env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_adt env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Newt_inst_data_newt (v1, v2, v3, v4) -> R.Case ("Newt_inst_data_newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = map_datainst env v3 in
      let v4 = map_newtype env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_idecl (env : env) (x : CST.idecl) =
  (match x with
  | `Func x -> R.Case ("Func",
      map_function_ env x
    )
  | `Sign x -> R.Case ("Sign",
      map_signature env x
    )
  | `Inst_data x -> R.Case ("Inst_data",
      map_inst_datainst env x
    )
  | `Inst_tyinst x -> R.Case ("Inst_tyinst",
      map_inst_tyinst env x
    )
  )

let map_topdecl (env : env) (x : CST.topdecl) =
  (match x with
  | `Decl_type (v1, v2, v3) -> R.Case ("Decl_type",
      let v1 = (* "type" *) token env v1 in
      let v2 = map_tyfam_head env v2 in
      let v3 =
        (match v3 with
        | `EQ_type_or_impl (v1, v2) -> R.Case ("EQ_type_or_impl",
            let v1 = (* "=" *) token env v1 in
            let v2 = map_type_or_implicit env v2 in
            R.Tuple [v1; v2]
          )
        | `Type_anno x -> R.Case ("Type_anno",
            map_type_annotation env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_tyfam (v1, v2, v3, v4, v5) -> R.Case ("Decl_tyfam",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "family" *) token env v2 in
      let v3 = map_tyfam_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            (match x with
            | `Type_anno x -> R.Case ("Type_anno",
                map_type_annotation env x
              )
            | `Tyfam_inj x -> R.Case ("Tyfam_inj",
                map_tyfam_inj env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* where *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  (match x with
                  | `LCURL_opt_tyfam_eq_rep_SEMI_tyfam_eq_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_tyfam_eq_rep_SEMI_tyfam_eq_opt_SEMI_RCURL",
                      let v1 = (* "{" *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some (v1, v2) -> R.Option (Some (
                            let v1 = map_tyfam_eq env v1 in
                            let v2 =
                              R.List (List.map (fun (v1, v2) ->
                                let v1 = (* ";" *) token env v1 in
                                let v2 = map_tyfam_eq env v2 in
                                R.Tuple [v1; v2]
                              ) v2)
                            in
                            R.Tuple [v1; v2]
                          ))
                        | None -> R.Option None)
                      in
                      let v3 =
                        (match v3 with
                        | Some tok -> R.Option (Some (
                            (* ";" *) token env tok
                          ))
                        | None -> R.Option None)
                      in
                      let v4 = (* "}" *) token env v4 in
                      R.Tuple [v1; v2; v3; v4]
                    )
                  | `Layout_start_opt_tyfam_eq_rep_choice_SEMI_tyfam_eq_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_tyfam_eq_rep_choice_SEMI_tyfam_eq_opt_choice_SEMI_layout_end",
                      let v1 = (* layout_start *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some (v1, v2, v3) -> R.Option (Some (
                            let v1 = map_tyfam_eq env v1 in
                            let v2 =
                              R.List (List.map (fun (v1, v2) ->
                                let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                                let v2 = map_tyfam_eq env v2 in
                                R.Tuple [v1; v2]
                              ) v2)
                            in
                            let v3 =
                              (match v3 with
                              | Some x -> R.Option (Some (
                                  map_anon_choice_SEMI_ab17175 env x
                                ))
                              | None -> R.Option None)
                            in
                            R.Tuple [v1; v2; v3]
                          ))
                        | None -> R.Option None)
                      in
                      let v3 = (* layout_end *) token env v3 in
                      R.Tuple [v1; v2; v3]
                    )
                  )
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Decl_tyinst (v1, v2, v3, v4, v5) -> R.Case ("Decl_tyinst",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "instance" *) token env v2 in
      let v3 = R.List (List.map (map_atype env) v3) in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_or_implicit env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Decl_role (v1, v2, v3, v4) -> R.Case ("Decl_role",
      let v1 = (* "type" *) token env v1 in
      let v2 = (* "role" *) token env v2 in
      let v3 = map_qtycon env v3 in
      let v4 = R.List (List.map (map_type_role env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Decl_adt (v1, v2, v3, v4, v5) -> R.Case ("Decl_adt",
      let v1 = (* "data" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_context_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_tyfam_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            (match x with
            | `Adt x -> R.Case ("Adt",
                map_adt env x
              )
            | `Rep_deri xs -> R.Case ("Rep_deri",
                R.List (List.map (map_deriving env) xs)
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Decl_newt (v1, v2, v3) -> R.Case ("Decl_newt",
      let v1 = (* "newtype" *) token env v1 in
      let v2 = map_context_newtype env v2 in
      let v3 =
        (match v3 with
        | `Newt x -> R.Case ("Newt",
            map_newtype env x
          )
        | `Opt_type_anno_gadt_rhs (v1, v2) -> R.Case ("Opt_type_anno_gadt_rhs",
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_type_annotation env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_gadt_rhs env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Decl_data_8db362d (v1, v2, v3, v4) -> R.Case ("Decl_data_8db362d",
      let v1 = (* "data" *) token env v1 in
      let v2 = (* "family" *) token env v2 in
      let v3 = map_tyfam_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_type_annotation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Decl_data_2e645bc x -> R.Case ("Decl_data_2e645bc",
      map_decl_datainst env x
    )
  | `Decl_import (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Decl_import",
      let v1 = (* "import" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "qualified" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* string *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = map_qmodid env v4 in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "qualified" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 = map_qmodid env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_import_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Decl_class (v1, v2, v3, v4, v5) -> R.Case ("Decl_class",
      let v1 = (* "class" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_context_ env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_constraint_ env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_fundeps env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_class_body env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Decl_inst (v1, v2) -> R.Case ("Decl_inst",
      let v1 = map_instance env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* where *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  (match x with
                  | `LCURL_opt_idecl_rep_SEMI_idecl_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_idecl_rep_SEMI_idecl_opt_SEMI_RCURL",
                      let v1 = (* "{" *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some (v1, v2) -> R.Option (Some (
                            let v1 = map_idecl env v1 in
                            let v2 =
                              R.List (List.map (fun (v1, v2) ->
                                let v1 = (* ";" *) token env v1 in
                                let v2 = map_idecl env v2 in
                                R.Tuple [v1; v2]
                              ) v2)
                            in
                            R.Tuple [v1; v2]
                          ))
                        | None -> R.Option None)
                      in
                      let v3 =
                        (match v3 with
                        | Some tok -> R.Option (Some (
                            (* ";" *) token env tok
                          ))
                        | None -> R.Option None)
                      in
                      let v4 = (* "}" *) token env v4 in
                      R.Tuple [v1; v2; v3; v4]
                    )
                  | `Layout_start_opt_idecl_rep_choice_SEMI_idecl_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_idecl_rep_choice_SEMI_idecl_opt_choice_SEMI_layout_end",
                      let v1 = (* layout_start *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some (v1, v2, v3) -> R.Option (Some (
                            let v1 = map_idecl env v1 in
                            let v2 =
                              R.List (List.map (fun (v1, v2) ->
                                let v1 = map_anon_choice_SEMI_ab17175 env v1 in
                                let v2 = map_idecl env v2 in
                                R.Tuple [v1; v2]
                              ) v2)
                            in
                            let v3 =
                              (match v3 with
                              | Some x -> R.Option (Some (
                                  map_anon_choice_SEMI_ab17175 env x
                                ))
                              | None -> R.Option None)
                            in
                            R.Tuple [v1; v2; v3]
                          ))
                        | None -> R.Option None)
                      in
                      let v3 = (* layout_end *) token env v3 in
                      R.Tuple [v1; v2; v3]
                    )
                  )
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Decl_defa (v1, v2, v3, v4) -> R.Case ("Decl_defa",
      let v1 = (* "default" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_type_infix env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* comma *) token env v1 in
                let v2 = map_type_infix env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Decl_fore x -> R.Case ("Decl_fore",
      map_decl_foreign env x
    )
  | `Decl_deri (v1, v2, v3) -> R.Case ("Decl_deri",
      let v1 = (* "deriving" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Deri_stra x -> R.Case ("Deri_stra",
                map_deriving_strategy env x
              )
            | `Via x -> R.Case ("Via",
                map_via env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = map_instance env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Decl x -> R.Case ("Decl",
      map_decl env x
    )
  | `Decl_pat (v1, v2) -> R.Case ("Decl_pat",
      let v1 = (* "pattern" *) token env v1 in
      let v2 =
        (match v2 with
        | `Pat_type x -> R.Case ("Pat_type",
            map_pattern_type env x
          )
        | `Pat_equals x -> R.Case ("Pat_equals",
            map_pattern_equals env x
          )
        | `Pat_arrow x -> R.Case ("Pat_arrow",
            map_pattern_arrow env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Top_splice x -> R.Case ("Top_splice",
      map_top_splice env x
    )
  )

let map_anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02 (env : env) ((v1, v2, v3) : CST.anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02) =
  let v1 = map_topdecl env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_anon_choice_SEMI_ab17175 env v1 in
      let v2 = map_topdecl env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_SEMI_ab17175 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_haskell (env : env) (x : CST.haskell) =
  (match x with
  | `Empty_file tok -> R.Case ("Empty_file",
      (* empty_file *) token env tok
    )
  | `Module (v1, v2, v3, v4, v5) -> R.Case ("Module",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_qmodid env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_exports env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* where *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            (match x with
            | `LCURL_opt_topd_rep_SEMI_topd_opt_SEMI_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_topd_rep_SEMI_topd_opt_SEMI_RCURL",
                let v1 = (* "{" *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some (v1, v2) -> R.Option (Some (
                      let v1 = map_topdecl env v1 in
                      let v2 =
                        R.List (List.map (fun (v1, v2) ->
                          let v1 = (* ";" *) token env v1 in
                          let v2 = map_topdecl env v2 in
                          R.Tuple [v1; v2]
                        ) v2)
                      in
                      R.Tuple [v1; v2]
                    ))
                  | None -> R.Option None)
                in
                let v3 =
                  (match v3 with
                  | Some tok -> R.Option (Some (
                      (* ";" *) token env tok
                    ))
                  | None -> R.Option None)
                in
                let v4 = (* "}" *) token env v4 in
                R.Tuple [v1; v2; v3; v4]
              )
            | `Layout_start_opt_topd_rep_choice_SEMI_topd_opt_choice_SEMI_layout_end (v1, v2, v3) -> R.Case ("Layout_start_opt_topd_rep_choice_SEMI_topd_opt_choice_SEMI_layout_end",
                let v1 = (* layout_start *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02 env x
                    ))
                  | None -> R.Option None)
                in
                let v3 = (* layout_end *) token env v3 in
                R.Tuple [v1; v2; v3]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Topd_rep_choice_SEMI_topd_opt_choice_SEMI x -> R.Case ("Topd_rep_choice_SEMI_topd_opt_choice_SEMI",
      map_anon_topd_rep_choice_SEMI_topd_opt_choice_SEMI_eb02f02 env x
    )
  )

let map_cpp (env : env) (tok : CST.cpp) =
  (* cpp *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_pragma (env : env) (tok : CST.pragma) =
  (* pragma *) token env tok

let dump_tree root =
  map_haskell () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Cpp (_loc, x) -> ("cpp", "cpp", map_cpp env x)
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Pragma (_loc, x) -> ("pragma", "pragma", map_pragma env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
