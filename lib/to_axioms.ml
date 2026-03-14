open Vernacexpr
open Decls
open Constrexpr
open Names
open Libnames

open Layout

exception ConvException of string


let spf = Printf.sprintf

let lident_to_string (l : lident) = Id.to_string l.v

let axiom_of_lname (c : lname) =
  match c.v with
  | Anonymous -> "_"
  | Name id -> Id.to_string id

let rec axiom_of_constr_expr (c : constr_expr) =
  match c.v with
  | CPrim p -> string_of_prim_token p
  | CRef (id, _) -> Id.to_string @@ qualid_basename id
  | CApp (f, args) ->
    let s = String.concat " " @@ List.map (fun (c, _) -> axiom_of_constr_expr c) args in
    spf "(%s %s)" (axiom_of_constr_expr f) s
  | CNotation (_, (_, "_ = _"), ([t1; t2], _, _, _)) -> spf "(%s == %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ + _"), ([t1; t2], _, _, _)) -> spf "(%s + %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ - _"), ([t1; t2], _, _, _)) -> spf "(%s - %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ * _"), ([t1; t2], _, _, _)) -> spf "(%s * %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ / _"), ([t1; t2], _, _, _)) -> spf "(%s / %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ < _"), ([t1; t2], _, _, _)) -> spf "(%s < %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ <= _"), ([t1; t2], _, _, _)) -> spf "(%s <= %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ > _"), ([t1; t2], _, _, _)) -> spf "(%s > %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ >= _"), ([t1; t2], _, _, _)) -> spf "(%s >= %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ /\\ _"), ([t1; t2], _, _, _)) -> spf "(%s && %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ \\/ _"), ([t1; t2], _, _, _)) -> spf "(%s || %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ -> _"), ([t1; t2], _, _, _)) -> spf "(%s #==> %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2)
  | CNotation (_, (_, "_ <-> _"), ([t1; t2], _, _, _)) -> spf "(iff %s %s)" (axiom_of_constr_expr t1) (axiom_of_constr_expr t2) (* TODO *)
  | CNotation (_, (_, "~ _"), ([t], _, _, _)) -> spf "(not %s)" @@ axiom_of_constr_expr t

  (* forall (n1 n2 ... : ty), e *)
  | CProdN ([(CLocalAssum (ns, _, _, ty))], e) ->
    let names = String.concat " " @@ List.map axiom_of_lname ns in
    spf "fun (%s : %s) -> %s" names (axiom_of_constr_expr ty) (axiom_of_constr_expr e)

  | CNotation (_, (_, "exists _ .. _ , _"), ([t], _, _, _)) ->
    spf "fun ((%s [@ex]) : %s) -> %s" "..." "..." @@ axiom_of_constr_expr t (* TODO: var name, type *)

  (* | CIf _ -> "(CIf ...)" *)
  (* | CFix _ -> "(CFix ...)" *)
  (* | CCoFix _ -> "(CCoFix ...)" *)
  (* | CLambdaN _ -> "(CLambdaN ...)" *)
  (* | CAppExpl _ -> "(CAppExpl ...)" *)
  (* | CEvar _ -> "(CEvar ...)" *)
  (* | CSort _ -> "(CSort ...)" *)
  (* | CCast _ -> "(CCast ...)" *)
  (* | CLetIn _ -> "(CLetIn ...)" *)
  (* | CCases _ -> "(CCases ...)" *)
  (* | CProj _ -> "(CProj ...)" *)
  (* | CRecord _ -> "(CRecord ...)" *)
  (* | CHole _ -> "(CHole ...)" *)
  (* | CGenarg _ -> "(CGenarg ...)" *)
  (* | CGenargGlob _ -> "(CGenargGlob ...)" *)
  (* | CPatVar _ -> "(CPatVar ...)" *)
  (* | CLetTuple _ -> "(CLetTuple ...)" *)
  (* | CGeneralization _ -> "(CGeneralization ...)" *)
  (* | CDelimiters _ -> "(CDelimiters ...)" *)
  (* | CArray _ -> "(CArray ...)" *)
  | _ -> "unknown constr" (* TODO: raise error *)

let axiom_of_assumption ax =
  match ax with
  | (_, ((id, _)::_, e)) ->
    let name = lident_to_string id in
    spf "let[@axiom] %s = %s" name @@ axiom_of_constr_expr e
  | _ -> raise @@ ConvException "Malformed axiom in VernacAssumption"

let rec axioms_of_parsed (start : bool) (ls : vernac_control list) =
  match ls with
  | [] -> []
  | { v = { expr = VernacSynterp (VernacDeclareModuleType (id, _, _, _)); _ }; _ }::xs when lident_to_string id = "Signatures" ->
    axioms_of_parsed true xs
  | { v = { expr = VernacSynterp (VernacEndSegment id); _ }; _ }::xs when lident_to_string id = "Signatures" ->
    []
  | { v = { expr = VernacSynPure (VernacAssumption ((NoDischarge, Logical), _, [q])) } }::xs when start -> (* Axiom *)
    axiom_of_assumption q :: axioms_of_parsed start xs
  | { v = { expr; _ }; _ }::xs -> axioms_of_parsed start xs (* Skip all other expressions *)

let axioms_of_parsed = axioms_of_parsed false
