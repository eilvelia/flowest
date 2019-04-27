open! Base

(* Maps Flow Ast to Ts Ast *)

module FlowAst = Flow_parser.Flow_ast
module TsAst = Ts.Ast
module FLoc = Flow_parser.Loc
module TLoc = Ts.Loc

let map_loc (loc: Flow_parser.Loc.t) = Ts.Loc.{
  start = { line = loc.start.line; column = loc.start.column };
  _end = { line = loc._end.line; column = loc._end.column };
}

let map_with_loc: 'a 'b. FLoc.t * 'a -> f:('a -> 'b) -> TLoc.t * 'b =
  fun (loc, node) ~f ->
    (map_loc loc, f node)

exception Error of Ts.Loc.t * string

let show_error ((loc, str): TLoc.t * string) =
  TLoc.to_string loc ^ " " ^ str

let error loc str = raise @@ Error (loc, str)
let errorf loc str = raise @@ Error (map_loc loc, str)
let errore str = raise @@ Error (TLoc.none, str)

let rec _none () = ()

and map_syntax
  : 'internal. (FLoc.t, 'internal) FlowAst.Syntax.t
  -> TLoc.t TsAst.Syntax.t
=
  fun t ->
    { TsAst.Syntax.
      leading = List.map t.leading ~f:map_comment;
      trailing = List.map t.trailing ~f:map_comment;
    }

and map_comment: FLoc.t FlowAst.Comment.t -> TLoc.t TsAst.Comment.t =
  let open TsAst.Comment in
  map_with_loc ~f:(fun (t': FlowAst.Comment.t') ->
    match t' with
    | Block str -> Block str
    | Line str -> Line str
  )

and map_identifier =
  map_with_loc ~f:(fun (t': FLoc.t FlowAst.Identifier.t') -> {
    TsAst.Identifier.
    name = t'.name;
    comments = Option.map t'.comments ~f:map_syntax;
  })

and map_class_implements =
  map_with_loc ~f:(fun (t': (FLoc.t, FLoc.t) FlowAst.Class.Implements.t') -> {
    TsAst.Class.Implements.
    id = map_identifier t'.id;
    targs = Option.map t'.targs ~f:map_type_parameter_instantiation;
  })

and map_private_name: FLoc.t FlowAst.PrivateName.t -> TLoc.t TsAst.PrivateName.t =
  map_with_loc ~f:map_identifier

and map_literal (t: FLoc.t FlowAst.Literal.t): TLoc.t TsAst.Literal.t =
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
  : (FLoc.t, FLoc.t) FlowAst.Type.Function.t
  -> (TLoc.t, TLoc.t) TsAst.Type.Function.t
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
        (map_loc loc', TsAst.Identifier.{ name = defaultname; comments = None })
    in
    let ts_t' = { Param.
      name;
      annot = map_type t'.annot;
      optional = t'.optional;
    } in
    (map_loc loc, ts_t')
  in
  let map_rest_param: _ FlowFn.RestParam.t -> _ RestParam.t =
    map_with_loc ~f:(fun (t': _ FlowFn.RestParam.t') ->
      { RestParam. argument = map_param t'.argument "args" })
  in
  let map_params: _ FlowFn.Params.t -> _ Params.t =
    map_with_loc ~f:(fun (t': _ FlowFn.Params.t') -> {
      Params.
      params = List.mapi t'.params ~f:(fun i x -> map_param x (char i));
      rest = Option.map t'.rest ~f:map_rest_param
    })
  in
  fun t ->
    {
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      params = map_params t.params;
      return = map_type t.return;
    }

and map_type_generic
  : (FLoc.t, FLoc.t) FlowAst.Type.Generic.t
  -> (TLoc.t, TLoc.t) TsAst.Type.Generic.t
=
  let open TsAst.Type.Generic in
  let module FlowGeneric = FlowAst.Type.Generic in
  let rec map_generic_id: (_, _) FlowGeneric.Identifier.t -> _ =
    let open Identifier in
    function
    | Unqualified id -> Unqualified (map_identifier id)
    | Qualified q -> Qualified (map_qualified q)
  and map_qualified: _ -> _ Identifier.qualified =
    map_with_loc ~f:(fun (q: _ FlowGeneric.Identifier.qualified') -> {
      Identifier.
      qualification = map_generic_id q.qualification;
      id = map_identifier q.id
    })
  in
  fun t ->
    {
      id = map_generic_id t.id;
      targs = Option.map t.targs ~f:map_type_parameter_instantiation
    }

and map_type_object
  : (FLoc.t, FLoc.t) FlowAst.Type.Object.t
  -> (TLoc.t, TLoc.t) TsAst.Type.Object.t
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
      | Literal (loc, l) -> Literal (map_loc loc, map_literal l)
      | Identifier id -> Identifier (map_identifier id)
      | PrivateName name -> PrivateName (map_private_name name)
      | Computed _ -> errorf loc "Computed properties aren't supported"
    in
    let map_value: (_, _) FlowObj.Property.value -> _ = function
      | Init t -> map_type t
      | Get (l, _) -> errorf l "No setter declaration in TS."
      | Set (l, _) -> errorf l "No getter declaration in TS."
    in
    let ts_t' = { Property.
      key = map_key t'.key;
      value = map_value t'.value;
      optional = t'.optional;
      static = t'.static;
      _method = t'._method;
      readonly = variance_opt t'.variance;
    } in
    (map_loc loc, ts_t')
  in
  let map_indexer ((loc, t'): (_, _) FlowObj.Indexer.t) =
    let id = match t'.id with
      | Some id -> map_identifier id
      | None -> (map_loc loc, TsAst.Identifier.{ name = "key"; comments = None })
    in
    let ts_t' = { Indexer.
      id;
      key = map_type t'.key;
      value = map_type t'.value;
      static = t'.static;
      readonly = variance_opt t'.variance;
    } in
    (map_loc loc, ts_t')
  in
  let map_call_property ((loc, t'): (_, _) FlowObj.CallProperty.t) =
    let value = map_with_loc t'.value ~f:map_type_function in
    let ts_t' = { CallProperty. value; static = t'.static } in
    (map_loc loc, ts_t')
  in
  let map_property: (_, _) FlowObj.property -> (_, _) property = function
    | Property p -> Property (map_common_property p)
    | SpreadProperty (l, _) -> errorf l "Spread properties aren't supported"
    | Indexer i -> Indexer (map_indexer i)
    | CallProperty p -> CallProperty (map_call_property p)
    | InternalSlot (l, _) -> errorf l "Internal slots aren't supported"
  in
  fun t ->
    { properties = List.map t.properties ~f:map_property }

and map_type_parameter_declaration
  : (FLoc.t, FLoc.t) FlowAst.Type.ParameterDeclaration.t
  -> (TLoc.t, TLoc.t) TsAst.Type.ParameterDeclaration.t
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
    (map_loc loc', ts_t'')
  in
  map_with_loc ~f:(List.map ~f:map_type_param)

and map_type_parameter_instantiation
  : (FLoc.t, FLoc.t) FlowAst.Type.ParameterInstantiation.t
  -> (TLoc.t, TLoc.t) TsAst.Type.ParameterInstantiation.t
=
  map_with_loc ~f:(List.map ~f:map_type)

and map_type
  ((loc, t'): (FLoc.t, FLoc.t) FlowAst.Type.t)
  : (TLoc.t, TLoc.t) TsAst.Type.t
=
  let open TsAst.Type in
  let tloc = map_loc loc in
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
    | Nullable t -> Union (map_type t, (tloc, Null), [(tloc, Undefined)])
    | Function f -> Function (map_type_function f)
    | Object o -> Object (map_type_object o)
    | Interface _ -> error tloc "No inline interfaces in TS."
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
    | Exists -> Any
  in
  (tloc, ts_t')

and map_type_annotation
  : (FLoc.t, FLoc.t) FlowAst.Type.annotation
  -> (TLoc.t, TLoc.t) TsAst.Type.t
=
  fun (_loc, t') ->
    map_type t'

and map_type_annotation_or_hint
  : (FLoc.t, FLoc.t) FlowAst.Type.annotation_or_hint
  -> (TLoc.t, TLoc.t) TsAst.Type.t option
=
  function
  | Missing _ -> None
  | Available annot -> Some (map_type_annotation annot)

and special_map_generic
  : (TLoc.t, TLoc.t) TsAst.Type.Generic.t
  -> (TLoc.t, TLoc.t) TsAst.Type.t'
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
      | "$ReadOnlyArray", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "ReadonlyArray"; comments }) (Some (loca, [t]))
      | "$ReadOnlyMap", Some (loca, [k; v]) ->
        gen_unq (locb, Id.{ name = "ReadonlyMap"; comments }) (Some (loca, [k; v]))
      | "$ReadOnlySet", Some (loca, [t]) ->
        gen_unq (locb, Id.{ name = "ReadonlySet"; comments }) (Some (loca, [t]))
      | _ ->
        Generic g

and map_stat_type_alias
  : (FLoc.t, FLoc.t) FlowAst.Statement.TypeAlias.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.TypeAlias.t
=
  fun t ->
    { TsAst.Statement.TypeAlias.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      right = map_type t.right;
    }

and map_stat_interface
  : (FLoc.t, FLoc.t) FlowAst.Statement.Interface.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.Interface.t
=
  fun t ->
    { TsAst.Statement.Interface.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      extends = List.map t.extends ~f:(map_with_loc ~f:map_type_generic);
      body = map_with_loc t.body ~f:map_type_object;
    }

and map_stat_declare_class
  : (FLoc.t, FLoc.t) FlowAst.Statement.DeclareClass.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.DeclareClass.t
=
  fun t ->
    { TsAst.Statement.DeclareClass.
      id = map_identifier t.id;
      tparams = Option.map t.tparams ~f:map_type_parameter_declaration;
      body = map_with_loc t.body ~f:map_type_object;
      (* TODO: handle 'mixins'? *)
      extends = Option.map t.extends ~f:(map_with_loc ~f:map_type_generic);
      implements = List.map t.implements ~f:map_class_implements;
    }

and map_stat_declare_variable
  : (FLoc.t, FLoc.t) FlowAst.Statement.DeclareVariable.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.DeclareVariable.t
=
  fun t ->
    { TsAst.Statement.DeclareVariable.
      id = map_identifier t.id;
      annot = map_type_annotation_or_hint t.annot;
    }

and map_stat_declare_function
  : (FLoc.t, FLoc.t) FlowAst.Statement.DeclareFunction.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.DeclareFunction.t
=
  fun t ->
    let (l, annot) = map_type_annotation t.annot in
    let annot = match annot with
      | Function f -> f
      | _ -> error l "Expected a function type in DeclareFunction"
    in
    { TsAst.Statement.DeclareFunction.
      id = map_identifier t.id;
      annot;
    }

and map_stat_export_named_declaration
  : (FLoc.t, FLoc.t) FlowAst.Statement.ExportNamedDeclaration.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.ExportNamedDeclaration.t
=
  let open TsAst.Statement.ExportNamedDeclaration in
  let module FlowExport = FlowAst.Statement.ExportNamedDeclaration in
  let map_export_specifier: _ -> _ ExportSpecifier.t =
    map_with_loc ~f:(fun (t': _ FlowExport.ExportSpecifier.t') -> {
      ExportSpecifier.
      local = map_identifier t'.local;
      exported = Option.map t'.exported ~f:map_identifier
    })
  in
  let map_specifier: _ FlowExport.specifier -> _ = function
    | ExportSpecifiers xs ->
      ExportSpecifiers (List.map xs ~f:map_export_specifier)
    | ExportBatchSpecifier (loc, id) ->
      ExportBatchSpecifier (map_loc loc, Option.map id ~f:map_identifier)
  in
  fun t ->
    {
      declaration = Option.map t.declaration ~f:map_statement;
      specifiers = Option.map t.specifiers ~f:map_specifier;
      source = Option.map t.source ~f:(map_with_loc ~f:map_string_literal);
    }

and map_stat_declare_export_declaration
  : (FLoc.t, FLoc.t) FlowAst.Statement.DeclareExportDeclaration.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.t'
=
  let open TsAst.Statement in
  let module FlowExport = FlowAst.Statement.DeclareExportDeclaration in
  let export_named d = ExportNamedDeclaration {
    declaration = Some d;
    specifiers = None;
    source = None;
  } in
  let map_declaration: _ FlowExport.declaration -> _ = function
    | Variable (l, s) -> map_loc l, DeclareVariable (map_stat_declare_variable s)
    | Function (l, s) -> map_loc l, DeclareFunction (map_stat_declare_function s)
    | Class (l, s) -> map_loc l, DeclareClass (map_stat_declare_class s)
    | DefaultType (l, _) -> errorf l "DefaultType isn't supported"
    | NamedType (l, s) -> map_loc l, DeclareTypeAlias (map_stat_type_alias s)
    | NamedOpaqueType (l, _) ->
      errorf l "'declare export opaque type' isn't supported"
    | Interface (l, s) -> map_loc l, DeclareInterface (map_stat_interface s)
  in
  fun t ->
    let flow_decl = match t.declaration with
      | None -> errore "'declare export' without 'declaration' isn't supported"
      | Some d -> d
    in
    let declaration = map_declaration flow_decl in
    match t.default with
    | None -> export_named declaration
    | Some l -> ExportDefaultDeclaration { default = map_loc l; declaration }

and map_stat_export_default_declaration
  : (FLoc.t, FLoc.t) FlowAst.Statement.ExportDefaultDeclaration.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.ExportDefaultDeclaration.t
=
  let open TsAst.Statement.ExportDefaultDeclaration in
  let module FlowExport = FlowAst.Statement.ExportDefaultDeclaration in
  let map_declaration: (_, _) FlowExport.declaration -> _ = function
    | Declaration s -> map_statement s
    | Expression (l, _) ->
      errorf l "Only statements are supported in 'export default'"
  in
  fun t ->
    {
      default = map_loc t.default;
      declaration = map_declaration t.declaration;
    }

and map_stat_import_declaration
  : (FLoc.t, FLoc.t) FlowAst.Statement.ImportDeclaration.t
  -> (TLoc.t, TLoc.t) TsAst.Statement.ImportDeclaration.t
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
      ImportNamespaceSpecifier (map_loc loc, map_identifier id)
  in
  fun t ->
    {
      source = map_with_loc t.source ~f:map_string_literal;
      default = Option.map t.default ~f:map_identifier;
      specifiers = Option.map t.specifiers ~f:map_specifier
    }

and map_statement
  ((loc, t'): (FLoc.t, FLoc.t) FlowAst.Statement.t)
  : (TLoc.t, TLoc.t) TsAst.Statement.t
=
  let open TsAst.Statement in
  let ts_t' = match t' with
    | DeclareClass s -> DeclareClass (map_stat_declare_class s)
    | DeclareExportDeclaration s -> map_stat_declare_export_declaration s
    | DeclareFunction s -> DeclareFunction (map_stat_declare_function s)
    | DeclareInterface s -> DeclareInterface (map_stat_interface s)
    | DeclareModule _s -> errorf loc "DeclareModule isn't supported"
    | DeclareModuleExports _s ->
      errorf loc "DeclareModuleExports isn't supported"
    | DeclareTypeAlias s -> DeclareTypeAlias (map_stat_type_alias s)
    | DeclareOpaqueType _s ->
      errorf loc "'declare opaque type' isn't supported" (* TODO: *)
    | DeclareVariable s -> DeclareVariable (map_stat_declare_variable s)
    | ExportDefaultDeclaration s ->
      ExportDefaultDeclaration (map_stat_export_default_declaration s)
    | ExportNamedDeclaration s ->
      ExportNamedDeclaration (map_stat_export_named_declaration s)
    | ImportDeclaration s -> ImportDeclaration (map_stat_import_declaration s)
    | InterfaceDeclaration s -> InterfaceDeclaration (map_stat_interface s)
    | TypeAlias s -> TypeAlias (map_stat_type_alias s)
    | OpaqueType _s -> errorf loc "'opaque type' isn't supported" (* TODO: *)
    | _ -> errorf loc "A statement isn't supported"
  in
  (map_loc loc, ts_t')

and map_program
  ((loc, stats, comments): (FLoc.t, FLoc.t) FlowAst.program)
  : (TLoc.t, TLoc.t) TsAst.program
= (
    map_loc loc,
    List.map stats ~f:map_statement,
    List.map comments ~f:map_comment
  )
