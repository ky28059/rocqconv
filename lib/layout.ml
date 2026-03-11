open Vernacexpr
open Decls
open Constrexpr
open Names
open Libnames

let spf = Printf.sprintf

let string_of_lident (l : lident) =
  "\"" ^ Id.to_string l.v ^ "\""

let string_of_qualid (q : qualid) =
  "\"" ^ (Id.to_string @@ qualid_basename q) ^ "\""

let layout_ident_decl (lid, _l) =
  "(" ^ string_of_lident lid ^ ", ...)"

let string_of_name (n : Name.t) =
  match n with
  | Name s -> spf "(Name \"%s\")" @@ Id.to_string s
  | Anonymous -> "Anonymous"

let string_of_prim_token p =
  match p with
  | Number n -> NumTok.Signed.sprint n
  | String s -> s

let string_of_notation_entry e =
  match e with
  | InConstrEntry -> "InConstrEntry"
  | InCustomEntry s -> spf "(InCustomEntry \"%s\")" s

let string_of_notation (e, n) =
  spf "(%s, \"%s\")" (string_of_notation_entry e) n

let rec string_of_constr_expr (c : constr_expr) =
  let string_of_constr_notation_subst (s, _, _, _) =
    let p = String.concat ", " @@ List.map string_of_constr_expr s in
    "([" ^ p ^ "], ..., ..., ...)" in
  match c.v with
  | CRef (id, _) -> spf "(CRef (%s, ...))" @@ string_of_qualid id
  | CFix _ -> "(CFix ...)"
  | CCoFix _ -> "(CCoFix ...)"
  | CProdN (_b, e) ->
    let s = "..." in (* TODO *)
    spf "(CProdN ([%s], %s))" s @@ string_of_constr_expr e
  | CLambdaN _ -> "(CLambdaN ...)"
  | CLetIn _ -> "(CLetIn ...)"
  | CAppExpl _ -> "(CAppExpl ...)"
  | CApp (f, args) ->
    let s = String.concat ", " @@ List.map (fun (c, _) -> spf "(%s, ...)" @@ string_of_constr_expr c) args in
    spf "(CApp (%s, [%s]))" (string_of_constr_expr f) s
  | CProj _ -> "(CProj ...)"
  | CRecord _ -> "(CRecord ...)"
  | CCases _ -> "(CCases ...)"
  | CLetTuple _ -> "(CLetTuple ...)"
  | CIf _ -> "(CIf ...)"
  | CHole _ -> "(CHole ...)"
  | CGenarg _ -> "(CGenarg ...)"
  | CGenargGlob _ -> "(CGenargGlob ...)"
  | CPatVar _ -> "(CPatVar ...)"
  | CEvar _ -> "(CEvar ...)"
  | CSort _ -> "(CSort ...)"
  | CCast _ -> "(CCast ...)"
  | CNotation (_, n, s) -> spf "(CNotation (..., %s, %s))" (string_of_notation n) (string_of_constr_notation_subst s)
  | CGeneralization _ -> "(CGeneralization ...)"
  | CPrim p -> spf "(CPrim %s)" @@ string_of_prim_token p
  | CDelimiters _ -> "(CDelimiters ...)"
  | CArray _ -> "(CArray ...)"

let string_of_proof_expr (id, (_, expr)) =
  spf "(%s, (..., %s))" (layout_ident_decl id) (string_of_constr_expr expr)

let string_of_theorem_kind t =
  match t with
  | Theorem -> "Theorem"
  | Lemma -> "Lemma"
  | Fact -> "Fact"
  | Remark -> "Remark"
  | Property -> "Property"
  | Proposition -> "Proposition"
  | Corollary -> "Corollary"

let string_of_assumption_kind t =
  match t with
  | Definitional -> "Parameter"
  | Logical -> "Axiom"
  | Conjectural -> "Conjecture"
  | Context -> "Context"

let string_of_discharge d =
  match d with
  | DoDischarge -> "DoDischarge"
  | NoDischarge -> "NoDischarge"

let string_of_bullet b =
  let open Proof_bullet in
  match b with
  | Dash n -> "(Dash " ^ string_of_int n ^ ")"
  | Star n -> "(Star " ^ string_of_int n ^ ")"
  | Plus n -> "(Plus " ^ string_of_int n ^ ")"

let string_of_pure p =
  match p with
  | VernacOpenCloseScope (b, name) -> spf "(VernacOpenCloseScope (%b, \"%s\"))" b name
  | VernacDeclareScope name -> spf "(VernacDeclareScope \"%s\")" name
  | VernacAssumption ((dis, kind), _, ls) ->
    let s = String.concat ", " @@ List.map (fun (_, (_, e)) -> spf "(..., (..., %s))" @@ string_of_constr_expr e) ls in
    spf "(VernacAssumption ((%s, %s), ..., [%s]))"
      (string_of_discharge dis)
      (string_of_assumption_kind kind)
      s
  (* | VernacFixpoint _ -> "" *)
  | VernacStartTheoremProof (kind, e) ->
    let p = String.concat ", " @@ List.map string_of_proof_expr e in
    spf "(VernacStartTheoremProof (%s, [%s]))" (string_of_theorem_kind kind) p
  | VernacProof (None, None) -> "(VernacProof (None, None)) (* Proof *)"
  | VernacProof (None, Some _) -> "(VernacProof (None, Some ...)) (* Proof using *)"
  | VernacProof (Some _, None) -> "(VernacProof (Some ..., None)) (* Proof with *)"
  | VernacProof (Some _, Some _) -> "(VernacProof (Some ..., Some ...)) (* Proof with using *)"
  | VernacExactProof p -> spf "(VernacExactProof %s)" @@ string_of_constr_expr p
  | VernacEndProof Admitted -> "(VernacEndProof Admitted)"
  | VernacEndProof (Proved (Transparent, None)) -> "(VernacEndProof (Proved (Transparent, None))) (* Defined *)"
  | VernacEndProof (Proved (Opaque, None)) -> "(VernacEndProof (Proved (Opaque, None))) (* Qed *)"
  | VernacEndProof (Proved (Opaque, Some l)) -> spf "(VernacEndProof (Proved (Opaque, Some %s)))" @@ string_of_lident l
  | VernacBullet b -> spf "(VernacBullet %s)" @@ string_of_bullet b
  | _ -> "(...)"

let string_of_synterp p =
  match p with
  | VernacLoad (_, s) -> spf "(VernacLoad (..., \"%s\"))" s
  | VernacReservedNotation (_, _) -> "(VernacReservedNotation ...)"
  | VernacNotation (_, _) -> "(VernacNotation ...)"
  | VernacDeclareCustomEntry s -> spf "(VernacDeclareCustomEntry \"%s\")" s
  | VernacBeginSection l -> spf "(VernacBeginSection %s)" @@ string_of_lident l
  | VernacEndSegment l -> spf "(VernacEndSegment %s)" @@ string_of_lident l
  | VernacRequire (id_opt, _, _) ->
    let id = match id_opt with
    | Some q -> spf "(Some %s)" @@ string_of_qualid q
    | None -> "None"
    in spf "(VernacRequire (%s, ..., ...)" id
  | VernacImport (_, _) -> "(VernacImport ...)"
  | VernacDeclareModule (_, l, _, _) -> spf "(VernacDeclareModule (..., %s, ..., ...))" @@ string_of_lident l
  | VernacDefineModule (_, l, _, _, _) -> spf "(VernacDefineModule (..., %s, ..., ..., ...)" @@ string_of_lident l
  | VernacDeclareModuleType (l, _, _, _) -> spf "(VernacDeclareModuleType (%s, ..., ..., ...)" @@ string_of_lident l
  | VernacInclude _ -> "(VernacInclude ...)"
  | VernacDeclareMLModule ls ->
    spf "(VernacDeclareMLModule [%s])" @@ String.concat ", " ls
  | VernacChdir None -> "(VernacChdir None)"
  | VernacChdir (Some dir) -> spf "(VernacChdir (Some \"%s\"))" dir
  | VernacExtraDependency (id, s, _) -> spf "(VernacExtraDependency (%s, \"%s\", ...))" (string_of_qualid id) s
  | VernacSetOption (b, _, _) -> spf "(VernacSetOption (%b, ..., ...))" b
  | VernacProofMode s -> spf "(VernacProofMode \"%s\")" s
  | VernacExtend (_, _) -> "(VernacExtend ...)" (* tactics *)

let string_of_expr expr =
  match expr with
  | VernacSynPure p -> "(VernacSynPure " ^ string_of_pure p ^ ")"
  | VernacSynterp p -> "(VernacSynterp " ^ string_of_synterp p ^ ")"

let string_of_vernac_ast ({ v = { expr; _ }; _ } : vernac_control) =
  string_of_expr expr
