open Ppxlib

(** Calculate length of a list, used to detect arity*)
let rec len l acc =
  match l with
    | [] -> acc
    | _::r -> len r (acc + 1)

(** Append the [@bs] attribute for ReScript compiler *)
let bs ~loc exp =
  let { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } = exp in
  let pexp_attributes = {
    attr_name={ txt="bs"; loc };
    attr_payload=PStr [];
    attr_loc=loc;
  }::pexp_attributes in
  { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }


let assertArgIsIdent arg =
  match arg with
    | { pexp_desc=Pexp_ident _; _ } -> ()
    | { pexp_desc=Pexp_send _; _} -> ()
    | { pexp_desc=Pexp_field _; _} -> ()
    | [%expr [%e? _]##[%e? _]] -> ()
    | { pexp_loc=loc; _ } -> Location.raise_errorf ~loc "Can only be identifier."


(** expands a series of function application with the corresponding expansion based on
    arity map. For example:
      suppose arityMap: 1 -> one | 2 -> two
      
    then, `f a b` becomes `((two f) a b) [@bs]`, and `f a` becomes `((one f) a) [@bs]`  *)
let expandFunctionApply ~loc arityMap arg body =
  let arity = len body 0 in
  let rec removeLabel l =
    match l with
      | [] -> []
      | (Nolabel, r)::rs -> r::removeLabel rs
      | (_, {pexp_loc=loc; _})::_ -> Location.raise_errorf ~loc "Labels are not supported"
  in
  let body = removeLabel body in
  let _ = assertArgIsIdent arg in
  Ast_builder.Default.(
    let { pexp_loc; _ } = arg in

    let txt = gen_symbol ~prefix:"precisely_" () in
    let p_app = ppat_var ~loc:pexp_loc { txt; loc=pexp_loc } in
    let e_app = pexp_ident ~loc:pexp_loc { txt=lident txt; loc=pexp_loc } in
    let app = eapply ~loc e_app body |> bs ~loc in
    [%expr let [%p p_app] = [%e arityMap arity] [%e arg] in [%e app]]
  )


let rec walk ~map ~push ~finalise ~acc records =
  match records with
    | [] -> finalise acc
    | elt::rest -> let mapped = map elt in walk ~map ~push ~finalise ~acc:(push mapped acc) rest
  
let both f g a = f a, g a

let pushTuple (e1, e2) (l1, l2) = e1::l1, e2::l2

let cons a al = a::al

let labelOfLident label =
  match label with
    | { txt = Lident txt; loc } -> { txt; loc }
    | { loc; _ } -> Location.raise_errorf ~loc "Unsupported token"
