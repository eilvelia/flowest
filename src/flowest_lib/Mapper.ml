open! Base
open Utils

module FlowAst = Flow_parser.Flow_ast
module TsAst = Ts.Ast

let ts_loc_of_flow_loc (loc: Flow_parser.Loc.t) = Ts.Loc.{
  start = { line = loc.start.line; column = loc.start.column };
  _end = { line = loc._end.line; column = loc._end.column };
}

exception Error of string

let error str = raise @@ Error str

let rec _none () = ()

and map_syntax: 'a 'internal. ('a, 'internal) FlowAst.Syntax.t -> 'a TsAst.Syntax.t =
  fun t ->
    { TsAst.Syntax.
      leading = List.map t.leading ~f:map_comment;
      trailing = List.map t.trailing ~f:map_comment;
    }

and map_comment: 'a. 'a FlowAst.Comment.t -> 'a TsAst.Comment.t = fun (loc, t') ->
  let open TsAst.Comment in
  let ts_t' = match t' with
    | Block str -> Block str
    | Line str -> Line str
  in
  (loc, ts_t')

and map_identifier: 'a. 'a FlowAst.Identifier.t -> 'a TsAst.Identifier.t =
  fun (loc, t') ->
    let ts_t' = {
      TsAst.Identifier.
      name = t'.name;
      comments = Option.map t'.comments ~f:map_syntax;
    } in
    (loc, ts_t')

and map_class_implements
  : 'a 'b. ('a, 'b) FlowAst.Class.Implements.t
  -> ('a, 'b) TsAst.Class.Implements.t
=
  fun (loc, t') ->
    let ts_t' = { TsAst.Class.Implements.
      id = map_identifier t'.id;
      targs = Option.map t'.targs ~f:map_type_parameter_instantiation;
    } in
    (loc, ts_t')

and map_private_name: 'a. 'a FlowAst.PrivateName.t -> 'a TsAst.PrivateName.t =
  fun (loc, id) ->
    (loc, map_identifier id)

and map_literal: 'a. 'a FlowAst.Literal.t -> 'a TsAst.Literal.t = fun t ->
  let open TsAst.Literal in
  let map_value: FlowAst.Literal.value -> value = function
    | String s -> String s
    | Boolean b -> Boolean b
    | Null -> Null
    | Number f -> Number f
    | RegExp r -> RegExp { pattern = r.pattern; flags = r.flags }
  in
  {
    value = map_value t.value;
    raw = t.raw;
    comments = Option.map t.comments ~f:map_syntax
  }

and map_string_literal (t: FlowAst.StringLiteral.t) = {
  TsAst.StringLiteral.
  value = t.value;
  raw = t.raw;
}

and map_number_literal (t: FlowAst.NumberLiteral.t) = {
  TsAst.NumberLiteral.
  value = t.value;
  raw = t.raw;
}

and map_type_function
  : 'a 'b. ('a, 'b) FlowAst.Type.Function.t
  -> ('a, 'b) TsAst.Type.Function.t
=
  let charset = "abcdefghijklmnopqrstuvwxyz" in
  let char (n: int) = String.of_char (charset.[n % String.length charset]) in
  let open TsAst.Type.Function in
  let module FlowFn = FlowAst.Type.Function in
  let map_param ((loc, t'): (_, _) FlowFn.Param.t) defaultname =
    let name = match t'.name with
      | Some n -> map_identifier n
      | None ->
        let (loc', _) = t'.annot in
        (loc', TsAst.Identifier.{ name = defaultname; comments = None })
    in
    let ts_t' = { Param.
      name;
      annot = map_type t'.annot;
      optional = t'.optional;
    } in
    (loc, ts_t')
  in
  let map_rest_param ((loc, t'): (_, _) FlowFn.RestParam.t) =
    let ts_t' = { RestParam. argument = map_param t'.argument "args" } in
    (loc, ts_t')
  in
  let map_params ((loc, t'): (_, _) FlowFn.Params.t) =
    let ts_t' = { Params.
      params = List.mapi t'.params ~f:(fun i x -> map_param x (char i));
      rest = Option.map t'.rest ~f:map_rest_param
    } in
    (loc, ts_t')
  in
  fun t ->
    {
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      params = map_params t.params;
      return = map_type t.return;
    }

and map_type_generic
  : 'a 'b. ('a, 'b) FlowAst.Type.Generic.t
  -> ('a, 'b) TsAst.Type.Generic.t
=
  let open TsAst.Type.Generic in
  let module FlowGeneric = FlowAst.Type.Generic in
  let rec map_generic_id: (_, _) FlowGeneric.Identifier.t -> _ =
    let open Identifier in
    function
    | Unqualified id -> Unqualified (map_identifier id)
    | Qualified q -> Qualified (map_qualified q)
  and map_qualified ((loc, q): (_, _) FlowGeneric.Identifier.qualified) =
    let ts_q = { Identifier.
      qualification = map_generic_id q.qualification;
      id = map_identifier q.id
    } in
    (loc, ts_q)
  in
  fun t ->
    {
      id = map_generic_id t.id;
      targs = Option.map t.targs ~f:map_type_parameter_instantiation
    }

and map_type_object
  : 'a 'b. ('a, 'b) FlowAst.Type.Object.t
  -> ('a, 'b) TsAst.Type.Object.t
=
  let open TsAst.Type.Object in
  let module FlowObj = FlowAst.Type.Object in
  let variance_opt: _ FlowAst.Variance.t option -> bool = function
    | Some (_, Plus) -> true
    | Some (_, Minus) | None -> false
  in
  let map_common_property ((loc, t'): (_, _) FlowObj.Property.t) =
    let map_key: (_, _) FlowAst.Expression.Object.Property.key -> _ =
      let open Property in
      function
      | Literal (loc, l) -> Literal (loc, map_literal l)
      | Identifier id -> Identifier (map_identifier id)
      | PrivateName name -> PrivateName (map_private_name name)
      | Computed _ -> error "Computed properties aren't supported"
    in
    let map_value: (_, _) FlowObj.Property.value -> _ = function
      | Init t -> map_type t
      | Get _ -> error "No setter declaration in TS."
      | Set _ -> error "No getter declaration in TS."
    in
    let ts_t' = { Property.
      key = map_key t'.key;
      value = map_value t'.value;
      optional = t'.optional;
      static = t'.static;
      _method = t'._method;
      readonly = variance_opt t'.variance;
    } in
    (loc, ts_t')
  in
  let map_indexer ((loc, t'): (_, _) FlowObj.Indexer.t) =
    let id = match t'.id with
      | Some id -> map_identifier id
      | None -> (loc, TsAst.Identifier.{ name = "key"; comments = None })
    in
    let ts_t' = { Indexer.
      id;
      key = map_type t'.key;
      value = map_type t'.value;
      static = t'.static;
      readonly = variance_opt t'.variance;
    } in
    (loc, ts_t')
  in
  let map_call_property ((loc, t'): (_, _) FlowObj.CallProperty.t) =
    let value = second t'.value ~f:map_type_function in
    let ts_t' = { CallProperty. value; static = t'.static } in
    (loc, ts_t')
  in
  let map_property: (_, _) FlowObj.property -> (_, _) property = function
    | Property p -> Property (map_common_property p)
    | SpreadProperty _ -> error "Spread properties aren't supported"
    | Indexer i -> Indexer (map_indexer i)
    | CallProperty p -> CallProperty (map_call_property p)
    | InternalSlot _ -> error "Internal slots aren't supported"
  in
  fun t ->
    { properties = List.map t.properties ~f:map_property }

and map_type_parameter_declaration
  : 'a 'b. ('a, 'b) FlowAst.Type.ParameterDeclaration.t
  -> ('a, 'b) TsAst.Type.ParameterDeclaration.t
=
  let open TsAst.Type.ParameterDeclaration in
  let module FlowTypeParam = FlowAst.Type.ParameterDeclaration.TypeParam in
  let map_type_param ((loc', t''): (_, _) FlowTypeParam.t) =
    let ts_t'' = {
      TypeParam.
      name = map_identifier t''.name;
      bound = map_type_annotation_or_hint t''.bound;
      default = Option.map t''.default ~f:map_type;
    } in
    (loc', ts_t'')
  in
  fun (loc, t') ->
    (loc, List.map t' ~f:map_type_param)

and map_type_parameter_instantiation
  : 'a 'b. ('a, 'b) FlowAst.Type.ParameterInstantiation.t
  -> ('a, 'b) TsAst.Type.ParameterInstantiation.t
=
  fun (loc, t') ->
    (loc, List.map t' ~f:map_type)

and map_type: 'a 'b. ('a, 'b) FlowAst.Type.t -> ('a, 'b) TsAst.Type.t = fun (loc, t') ->
  let open TsAst.Type in
  (* Note that the left constructors are from FlowAst, but the right are from TsAst *)
  let ts_t' = match t' with
    | Any -> Any
    | Mixed -> Unknown
    | Empty -> Never
    | Void -> Void
    | Null -> Null
    | Number -> Number
    | String -> String
    | Boolean -> Boolean
    | Nullable t -> Union (map_type t, (loc, Null), [(loc, Undefined)])
    | Function f -> Function (map_type_function f)
    | Object o -> Object (map_type_object o)
    | Interface _ -> error "No inline interfaces in TS."
    | Array t -> Array (map_type t)
    | Generic g -> special_map_generic @@ map_type_generic g
    | Union (t1, t2, ts) ->
      Union (map_type t1, map_type t2, List.map ts ~f:map_type)
    | Intersection (t1, t2, ts) ->
      Intersection (map_type t1, map_type t2, List.map ts ~f:map_type)
    | Typeof t -> Typeof (map_type t)
    | Tuple ts -> Tuple (List.map ts ~f:map_type)
    | StringLiteral l -> StringLiteral (map_string_literal l)
    | NumberLiteral l -> NumberLiteral (map_number_literal l)
    | BooleanLiteral b -> BooleanLiteral b
    | Exists -> error "No '*' in TS."
  in
  (loc, ts_t')

and map_type_annotation
  : 'a 'b. ('a, 'b) FlowAst.Type.annotation
  -> ('a, 'b) TsAst.Type.t
=
  fun (_loc, t') ->
    map_type t'

and map_type_annotation_or_hint
  : 'a 'b. ('a, 'b) FlowAst.Type.annotation_or_hint
  -> ('a, 'b) TsAst.Type.t option
=
  function
  | Missing _ -> None
  | Available annot -> Some (map_type_annotation annot)

and special_map_generic
  : 'a 'b. ('a, 'b) TsAst.Type.Generic.t
  -> ('a, 'b) TsAst.Type.t'
=
  let open TsAst in
  let open Type in
  let module Id = Identifier in
  let gen_unq id targs = (* Generic Unqualified *)
    Generic { id = Generic.Identifier.Unqualified id; targs } in
  fun g -> match g.id with
    | Qualified _ -> Generic g
    | Unqualified id ->
      let (locb, id') = id in
      let Id.{ comments; _ } = id' in
      match id'.name, g.targs with
      | "$Rest", Some (loca, [t; _, Object { properties = [] }]) ->
        gen_unq (locb, Id.{ name = "Partial"; comments }) (Some (loca, [t]))
      | "$ReadOnly", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "Readonly"; comments }) (Some (loca, [t]))
      | "$ReadOnlyArray", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "ReadonlyArray"; comments }) (Some (loca, [t]))
      | "$Keys", Some (_loca, [t]) ->
        Keyof t
      | "$Values", Some (_loca, [t]) ->
        IndexedAccess (t, (locb, Keyof t))
      | "$Exact", Some (_loca, [t]) ->
        snd t
      | "Class", Some (_loca, [t]) ->
        Typeof t
      | "$PropertyType", Some (_loca, [o; k])
      | "$ElementType", Some (_loca, [o; k]) ->
        IndexedAccess (o, k)
      | "$Call", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "ReturnType"; comments }) (Some (loca, [t]))
      | "$NonMaybeType", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "NonNullable"; comments }) (Some (loca, [t]))
      | _ ->
        Generic g

and map_stat_type_alias
  : 'a 'b. ('a, 'b) FlowAst.Statement.TypeAlias.t
  -> ('a, 'b) TsAst.Statement.TypeAlias.t
=
  fun t ->
    { TsAst.Statement.TypeAlias.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      right = map_type t.right;
    }

and map_stat_interface
  : 'a 'b. ('a, 'b) FlowAst.Statement.Interface.t
  -> ('a, 'b) TsAst.Statement.Interface.t
=
  fun t ->
    { TsAst.Statement.Interface.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      extends = List.map t.extends ~f:(second ~f:map_type_generic);
      body = second t.body ~f:map_type_object;
    }

and map_stat_declare_class
  : 'a 'b. ('a, 'b) FlowAst.Statement.DeclareClass.t
  -> ('a, 'b) TsAst.Statement.DeclareClass.t
=
  fun t ->
    { TsAst.Statement.DeclareClass.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      body = second t.body ~f:map_type_object;
      (* TODO: handle 'mixins'? *)
      extends = Option.map t.extends ~f:(second ~f:map_type_generic);
      implements = List.map t.implements ~f:map_class_implements;
    }

and map_stat_declare_variable
  : 'a 'b. ('a, 'b) FlowAst.Statement.DeclareVariable.t
  -> ('a, 'b) TsAst.Statement.DeclareVariable.t
=
  fun t ->
    { TsAst.Statement.DeclareVariable.
      id = map_identifier t.id;
      annot = map_type_annotation_or_hint t.annot;
    }

and map_stat_declare_function
  : 'a 'b. ('a, 'b) FlowAst.Statement.DeclareFunction.t
  -> ('a, 'b) TsAst.Statement.DeclareFunction.t
=
  fun t ->
    let annot = match t.annot |> map_type_annotation |> snd with
      | Function f -> f
      | _ -> error "Expected a function type in DeclareFunction"
    in
    { TsAst.Statement.DeclareFunction.
      id = map_identifier t.id;
      annot;
    }

and map_stat_export_named_declaration
  : 'a 'b. ('a, 'b) FlowAst.Statement.ExportNamedDeclaration.t
  -> ('a, 'b) TsAst.Statement.ExportNamedDeclaration.t
=
  let open TsAst.Statement.ExportNamedDeclaration in
  let module FlowExport = FlowAst.Statement.ExportNamedDeclaration in
  let map_export_specifier ((loc, t'): _ FlowExport.ExportSpecifier.t) =
    let ts_t' = { ExportSpecifier.
      local = map_identifier t'.local;
      exported = Option.map t'.exported ~f:map_identifier
    } in
    (loc, ts_t')
  in
  let map_specifier: _ FlowExport.specifier -> _ = function
    | ExportSpecifiers xs ->
      ExportSpecifiers (List.map xs ~f:map_export_specifier)
    | ExportBatchSpecifier (loc, id) ->
      ExportBatchSpecifier (loc, Option.map id ~f:map_identifier)
  in
  fun t ->
    {
      declaration = Option.map t.declaration ~f:map_statement;
      specifiers = Option.map t.specifiers ~f:map_specifier;
      source = Option.map t.source ~f:(second ~f:map_string_literal);
    }

and map_stat_declare_export_declaration
  : 'a 'b. ('a, 'b) FlowAst.Statement.DeclareExportDeclaration.t
  -> ('a, 'b) TsAst.Statement.t'
=
  let open TsAst.Statement in
  let module FlowExport = FlowAst.Statement.DeclareExportDeclaration in
  let export_named d = ExportNamedDeclaration {
    declaration = Some d;
    specifiers = None;
    source = None;
  } in
  let map_declaration: _ FlowExport.declaration -> _ = function
    | Variable (l, s) -> l, DeclareVariable (map_stat_declare_variable s)
    | Function (l, s) -> l, DeclareFunction (map_stat_declare_function s)
    | Class (l, s) -> l, DeclareClass (map_stat_declare_class s)
    | DefaultType _ -> error "DefaultType isn't supported"
    | NamedType (l, s) -> l, DeclareTypeAlias (map_stat_type_alias s)
    | NamedOpaqueType _ -> error "'declare export opaque type' isn't supported"
    | Interface (l, s) -> l, DeclareInterface (map_stat_interface s)
  in
  fun t ->
    let flow_decl = match t.declaration with
      | None -> error "'declare export' without 'declaration' isn't supported"
      | Some d -> d
    in
    let declaration = map_declaration flow_decl in
    match t.default with
    | None -> export_named declaration
    | Some default -> ExportDefaultDeclaration { default; declaration }

and map_stat_export_default_declaration
  : 'a 'b. ('a, 'b) FlowAst.Statement.ExportDefaultDeclaration.t
  -> ('a, 'b) TsAst.Statement.ExportDefaultDeclaration.t
=
  let open TsAst.Statement.ExportDefaultDeclaration in
  let module FlowExport = FlowAst.Statement.ExportDefaultDeclaration in
  let map_declaration: (_, _) FlowExport.declaration -> _ = function
    | Declaration s -> map_statement s
    | Expression _ ->
      error "Only statements are supported in 'export default'"
  in
  fun t ->
    {
      default = t.default;
      declaration = map_declaration t.declaration;
    }

and map_stat_import_declaration
  : 'a 'b. ('a, 'b) FlowAst.Statement.ImportDeclaration.t
  -> ('a, 'b) TsAst.Statement.ImportDeclaration.t
=
  let open TsAst.Statement.ImportDeclaration in
  let module FlowImport = FlowAst.Statement.ImportDeclaration in
  let map_named_specifier (s: _ FlowImport.named_specifier) = {
    local = Option.map s.local ~f:map_identifier;
    remote = map_identifier s.remote;
  } in
  let map_specifier: (_, _) FlowImport.specifier -> _ = function
    | ImportNamedSpecifiers xs ->
      ImportNamedSpecifiers (List.map xs ~f:map_named_specifier)
    | ImportNamespaceSpecifier (loc, id) ->
      ImportNamespaceSpecifier (loc, map_identifier id)
  in
  fun t ->
    {
      source = second t.source ~f:map_string_literal;
      default = Option.map t.default ~f:map_identifier;
      specifiers = Option.map t.specifiers ~f:map_specifier
    }

and map_statement
  : 'a 'b. ('a, 'b) FlowAst.Statement.t
  -> ('a, 'b) TsAst.Statement.t
=
  let open TsAst.Statement in
  fun (loc, t') ->
    let ts_t' = match t' with
      | DeclareClass s -> DeclareClass (map_stat_declare_class s)
      | DeclareExportDeclaration s -> map_stat_declare_export_declaration s
      | DeclareFunction s -> DeclareFunction (map_stat_declare_function s)
      | DeclareInterface s -> DeclareInterface (map_stat_interface s)
      | DeclareModule _s -> error "DeclareModule isn't supported"
      | DeclareModuleExports _s -> error "DeclareModuleExports isn't supported"
      | DeclareTypeAlias s -> DeclareTypeAlias (map_stat_type_alias s)
      | DeclareOpaqueType _s ->
        error "'declare opaque type' isn't supported" (* TODO: *)
      | DeclareVariable s -> DeclareVariable (map_stat_declare_variable s)
      | ExportDefaultDeclaration s ->
        ExportDefaultDeclaration (map_stat_export_default_declaration s)
      | ExportNamedDeclaration s ->
        ExportNamedDeclaration (map_stat_export_named_declaration s)
      | ImportDeclaration s -> ImportDeclaration (map_stat_import_declaration s)
      | InterfaceDeclaration s -> InterfaceDeclaration (map_stat_interface s)
      | TypeAlias s -> TypeAlias (map_stat_type_alias s)
      | OpaqueType _s -> error "'opaque type' isn't supported" (* TODO: *)
      | _ -> error "A statement isn't supported"
    in
    (loc, ts_t')

and map_program: 'a 'b. ('a, 'b) FlowAst.program -> ('a, 'b) TsAst.program =
  fun (loc, stats, comments) ->
    (loc, List.map stats ~f:map_statement, List.map comments ~f:map_comment)
