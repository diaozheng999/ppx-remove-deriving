open Ppxlib

let pattern () = Ast_pattern.(single_expr_payload __)

let build_single_expr ~loc e =
  PStr [ Ast_builder.Default.(pstr_eval ~loc e []) ]

(** Source: https://rescript-lang.org/docs/manual/latest/generate-converters-accessors *)
let should_keep pat =
  match pat with
  | [%expr { jsConverter = [%e? _] }] -> true
  | [%expr jsConverter] -> true
  | [%expr accessors] -> true
  | [%expr abstract] -> true
  | _ -> false

let normalize_deriving pat =
  match pat with { pexp_desc = Pexp_tuple t; _ } -> t | _ -> [ pat ]

let concat_result ~loc deriving acc result =
  let open Ast_builder.Default in
  let attach expr =
    let payload = build_single_expr ~loc expr in
    attribute ~loc ~name:deriving ~payload :: acc
  in

  match result with
  | [] -> acc
  | [ exp ] -> attach exp
  | expl ->
      let tup = Ast_builder.Default.pexp_tuple ~loc expl in
      attach tup

let transform pat =
  let normalized = normalize_deriving pat in
  List.filter should_keep normalized

let rec fold_attr attr acc =
  match attr with
  | [] -> List.rev acc
  | {
      attr_name = { txt = "deriving"; _ } as deriving;
      attr_payload;
      attr_loc = loc;
    }
    :: r ->
      let result = Ast_pattern.parse (pattern ()) loc attr_payload transform in
      fold_attr r (concat_result ~loc deriving acc result)
  | x :: r -> fold_attr r (x :: acc)

let traverse =
  object (self)
    inherit Ppxlib.Ast_traverse.map as super

    method! type_declaration
        {
          ptype_attributes;
          ptype_name;
          ptype_cstrs;
          ptype_kind;
          ptype_private;
          ptype_manifest;
          ptype_params;
          ptype_loc;
        } =
      {
        ptype_attributes = fold_attr ptype_attributes [];
        ptype_name;
        ptype_cstrs;
        ptype_kind;
        ptype_private;
        ptype_manifest;
        ptype_params;
        ptype_loc;
      }
  end

let () =
  Driver.register_transformation ~impl:traverse#structure
    ~intf:traverse#signature "remove-deriving"
