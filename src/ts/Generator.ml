open! Base

module rec Syntax: sig
  val gen_comment_list: 'a Ast.Comment.t list -> string
  val gen_opt: 'a Ast.Syntax.t option -> (string * string)
end = struct
  let gen_comment_list (l: _ Ast.Comment.t list) =
    List.fold l ~init:"" ~f:(fun acc c -> acc ^ " " ^ Comment.gen c)
  let gen_opt (v: _ Ast.Syntax.t option) = match v with
    | Some c ->
      (gen_comment_list c.leading ^ " ", " " ^ gen_comment_list c.trailing)
    | None ->
      ("", "")
end

and Comment: sig
  val gen: 'a Ast.Comment.t -> string
end = struct
  let gen ((_, t'): _ Ast.Comment.t) = match t' with
    | Block str -> "/*" ^ str ^ "*/"
    | Line str -> "//" ^ str
end

and Identifier: sig
  val gen: 'a Ast.Identifier.t -> string
end = struct
  let gen ((_, t'): _ Ast.Identifier.t) =
    (* let (leading, trailing) = Syntax.gen_opt t'.comments in
    leading ^ t'.name ^ trailing *)
    t'.name
end

and Class: sig
  module Implements: sig
    val gen: ('a, 'b) Ast.Class.Implements.t -> string

  end
end = struct
  module Implements = struct
    let gen ((_, t'): (_, _) Ast.Class.Implements.t) =
      let id = Identifier.gen t'.id in
      let targs = Type.ParameterInstantiation.gen_opt t'.targs in
      id ^ targs
  end
end

and PrivateName: sig
  val gen: 'a Ast.PrivateName.t -> string
end = struct
  let gen ((_, id): _ Ast.PrivateName.t) =
    "#" ^ Identifier.gen id
end

and Literal: sig
  module RegExp: sig
    val gen: Ast.Literal.RegExp.t -> string
  end
  val gen: 'a Ast.Literal.t -> string
end = struct
  module RegExp = struct
    let gen (t: Ast.Literal.RegExp.t) =
      "/" ^ t.pattern ^ "/" ^ t.flags
  end
  let gen (t: _ Ast.Literal.t) =
    (* let (leading, trailing) = Syntax.gen_opt t.comments in
    leading ^ t.raw ^ trailing *)
    t.raw
end

and StringLiteral: sig
  val gen: Ast.StringLiteral.t -> string
end = struct
  let gen (t: Ast.StringLiteral.t) =
    t.raw
end

and NumberLiteral: sig
  val gen: Ast.NumberLiteral.t -> string
end = struct
  let gen (t: Ast.NumberLiteral.t) =
    t.raw
end

and Type: sig
  module Function: sig
    val gen: ?arrow:bool -> ?no_return:bool -> ('a, 'b) Ast.Type.Function.t -> string
  end
  module Generic: sig
    val gen: ('a, 'b) Ast.Type.Generic.t -> string
  end
  module Object: sig
    val gen: ('a, 'b) Ast.Type.Object.t -> string
  end
  module ParameterDeclaration: sig
    val gen: ('a, 'b) Ast.Type.ParameterDeclaration.t -> string
    val gen_opt: ('a, 'b) Ast.Type.ParameterDeclaration.t option -> string
  end
  module ParameterInstantiation: sig
    val gen: ('a, 'b) Ast.Type.ParameterInstantiation.t -> string
    val gen_opt: ('a, 'b) Ast.Type.ParameterInstantiation.t option -> string
  end
  val gen: ('a, 'b) Ast.Type.t -> string
end = struct
  module Function = struct
    module Param = struct
      let gen ((_, t'): (_, _) Ast.Type.Function.Param.t) =
        let name = Identifier.gen t'.name in
        let annot = Type.gen t'.annot in
        let opt = if t'.optional then "?" else "" in
        name ^ opt ^ ":" ^ " " ^ annot
    end
    module RestParam = struct
      let gen ((_, t'): (_, _) Ast.Type.Function.RestParam.t) =
        "..." ^ Param.gen t'.argument
    end
    module Params = struct
      let gen ((_, t'): (_, _) Ast.Type.Function.Params.t) =
        let params = List.map t'.params ~f:Param.gen in
        let rest = Option.(t'.rest >>| RestParam.gen) in
        let rest_list = Option.value_map rest ~default:[] ~f:List.return in
        let all_params = params @ rest_list in
        "(" ^ String.concat ~sep:", " all_params ^ ")"
    end
    let gen ?(arrow = false) ?(no_return = false) (t: (_, _) Ast.Type.Function.t) =
      let tparams = Type.ParameterDeclaration.gen_opt t.tparams in
      let params = Params.gen t.params in
      let return_t = Type.gen t.return in
      let arrow = if arrow then " => " else ": " in
      if no_return
      then tparams ^ params
      else tparams ^ params ^ arrow ^ return_t
  end

  module Generic = struct
    module Identifier = struct
      let rec gen (t: (_, _) Ast.Type.Generic.Identifier.t) =
        match t with
        | Unqualified x -> Identifier.gen x
        | Qualified x -> gen_qualified x
      and gen_qualified ((_, t'): (_, _) Ast.Type.Generic.Identifier.qualified) =
        let qualification = gen t'.qualification in
        let id = Identifier.gen t'.id in
        qualification ^ "." ^ id
    end
    let gen (t: (_, _) Ast.Type.Generic.t) =
      let id = Identifier.gen t.id in
      let targs = Option.value_map t.targs
        ~default:"" ~f:Type.ParameterInstantiation.gen in
      id ^ targs
  end

  module Object = struct
    module Property = struct
      let gen_key: (_, _) Ast.Type.Object.Property.key -> string = function
        | Literal (_, l) -> Literal.gen l
        | Identifier id -> Identifier.gen id
        | PrivateName n -> PrivateName.gen n
      let gen ((_, t'): (_, _) Ast.Type.Object.Property.t) =
        let key = gen_key t'.key in
        let opt = if t'.optional then "?" else "" in
        let static = if t'.static then "static " else "" in
        let readonly = if t'.readonly then "readonly " else "" in
        let met = if t'._method then "" else ": " in
        let value =
          if t'._method
          then match t'.value with
            | (_, Ast.Type.Function f) ->
              let no_return = String.(key = "constructor") in
              Type.Function.gen ~no_return f
            | _ -> failwith "Ast.Type.Function expected"
          else Type.gen t'.value
        in
        static ^ readonly ^ key ^ opt ^ met ^ value
    end

    module CallProperty = struct
      let gen ((_, t'): (_, _) Ast.Type.Object.CallProperty.t) =
        let static = if t'.static then "static " else "" in
        let (_, value) = t'.value in
        let value = Function.gen value in
        static ^ value
    end

    module Indexer = struct
      let gen ((_, t'): (_, _) Ast.Type.Object.Indexer.t) =
        let id = Identifier.gen t'.id in
        let key = Type.gen t'.key in
        let value = Type.gen t'.value in
        let static = if t'.static then "static " else "" in
        let readonly = if t'.readonly then "readonly " else "" in
        static ^ readonly ^ "[" ^ id ^ ": " ^ key ^ "]: " ^ value
    end

    let gen_property: (_, _) Ast.Type.Object.property -> string = function
      | Property p -> Property.gen p
      | Indexer p -> Indexer.gen p
      | CallProperty p -> CallProperty.gen p

    let gen (t: (_, _) Ast.Type.Object.t) =
      let properties =
        List.map t.properties ~f:gen_property |> String.concat ~sep:"; " in
      "{ " ^ properties ^ " }"
  end

  module ParameterDeclaration = struct
    module TypeParam = struct
      let gen ((_, t'): (_, _) Ast.Type.ParameterDeclaration.TypeParam.t) =
        let name = Identifier.gen t'.name in
        let bound = match t'.bound with
          | Some constr -> " extends " ^ Type.gen constr
          | None -> ""
        in
        let default = match t'.default with
          | Some d -> " = " ^ Type.gen d
          | None -> ""
        in
        name ^ bound ^ default
    end
    let gen ((_, t'): (_, _) Ast.Type.ParameterDeclaration.t) =
      let tparams = List.map t' ~f:TypeParam.gen |> String.concat ~sep:", " in
      "<" ^ tparams ^ ">"

    let gen_opt (o: (_, _) Ast.Type.ParameterDeclaration.t option) =
      Option.value_map o ~default:"" ~f:gen
  end

  module ParameterInstantiation = struct
    let gen ((_, t'): (_, _) Ast.Type.ParameterInstantiation.t) =
      let types = List.map t' ~f:Type.gen |> String.concat ~sep:", " in
      "<" ^ types ^ ">"

    let gen_opt (o: (_, _) Ast.Type.ParameterInstantiation.t option) =
      Option.value_map o ~default:"" ~f:gen
  end

  let rec gen ((_, t'): (_, _) Ast.Type.t) = match t' with
    | Any -> "any"
    | Unknown -> "unknown"
    | Never -> "never"
    | Void -> "void"
    | Undefined -> "undefined"
    | Null -> "null"
    | Number -> "number"
    | String -> "string"
    | Boolean -> "boolean"
    | Function f -> "(" ^ Function.gen ~arrow:true f ^ ")"
    | Object o -> Object.gen o
    | Array t -> "(" ^ gen t ^ ")[]"
    | Generic t -> Generic.gen t
    | Union (t1, t2, tl) ->
      let str = List.map (t1 :: t2 :: tl) ~f:gen |> String.concat ~sep:" | " in
      "(" ^ str ^ ")"
    | Intersection (t1, t2, tl) ->
      let str = List.map (t1 :: t2 :: tl) ~f:gen |> String.concat ~sep:" & " in
      "(" ^ str ^ ")"
    | Typeof t -> "typeof " ^ gen t
    | Tuple ts -> "[" ^ String.concat ~sep:", " (List.map ts ~f:gen) ^ "]"
    | StringLiteral l -> StringLiteral.gen l
    | NumberLiteral l -> NumberLiteral.gen l
    | BooleanLiteral b -> if b then "true" else "false"
    | Keyof t -> "keyof " ^ gen t
    | Unique t -> "unique " ^ gen t
    | Readonly t -> "readonly " ^ gen t
    | IndexedAccess (o, i) -> gen o ^ "[" ^ gen i ^ "]"
    | InlineTs str -> str
end

and Statement: sig
  val gen: ?no_declare:bool -> ('a, 'b) Ast.Statement.t -> string
end = struct
  module TypeAlias = struct
    let gen (t: (_, _) Ast.Statement.TypeAlias.t) =
      let id = Identifier.gen t.id in
      let tparams = Type.ParameterDeclaration.gen_opt t.tparams in
      let right = Type.gen t.right in
      "type " ^ id ^ tparams ^ " = " ^ right
  end

  module Interface = struct
    let gen (t: (_, _) Ast.Statement.Interface.t) =
      let id = Identifier.gen t.id in
      let tparams = Type.ParameterDeclaration.gen_opt t.tparams in
      let extends_list = List.map t.extends
        ~f:(fun (_, x) -> Type.Generic.gen x) in
      let extends = match extends_list with
        | [] -> ""
        | xs -> "extends " ^ String.concat ~sep:", " xs ^ " "
      in
      let (_, body) = t.body in
      let body = Type.Object.gen body in
      "interface " ^ id ^ tparams ^ " " ^ extends ^ body
  end

  module DeclareClass = struct
    let gen ~no_declare (t: (_, _) Ast.Statement.DeclareClass.t) =
      let id = Identifier.gen t.id in
      let tparams = Type.ParameterDeclaration.gen_opt t.tparams in
      let (_, body) = t.body in
      let body = Type.Object.gen body in
      let extends = match t.extends with
        | None -> ""
        | Some (_, g) -> "extends " ^ Type.Generic.gen g ^ " "
      in
      let implements_list = List.map t.implements ~f:Class.Implements.gen in
      let implements = match implements_list with
        | [] -> ""
        | xs -> "implements " ^ String.concat ~sep:", " xs ^ " "
      in
      let declare = if no_declare then "" else "declare " in
      declare ^ "class " ^ id ^ tparams ^ " " ^ extends ^ implements ^ body
  end

  module DeclareVariable = struct
    let gen (t: (_, _) Ast.Statement.DeclareVariable.t) =
      let id = Identifier.gen t.id in
      let annot = match t.annot with
        | Some x -> ": " ^ Type.gen x
        | None -> ""
      in
      "declare var " ^ id ^ annot
  end

  module DeclareFunction = struct
    let gen ~no_declare (t: (_, _) Ast.Statement.DeclareFunction.t) =
      let id = Identifier.gen t.id in
      let annot = Type.Function.gen t.annot in
      let declare = if no_declare then "" else "declare " in
      declare ^ "function " ^ id ^ annot
  end

  module ExportNamedDeclaration = struct
    module ExportSpecifier = struct
      let gen ((_, t'): _ Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t) =
        let local = Identifier.gen t'.local in
        let exported = match t'.exported with
          | Some id -> " as " ^ Identifier.gen id
          | None -> ""
        in
        local ^ exported
    end

    let gen_specifier (s: _ Ast.Statement.ExportNamedDeclaration.specifier) =
      match s with
        | ExportSpecifiers xs ->
          let str = List.map xs ~f:ExportSpecifier.gen |> String.concat ~sep:", " in
          "{ " ^ str ^ " }"
        | ExportBatchSpecifier (_, Some id) ->
          "* as " ^ Identifier.gen id
        | ExportBatchSpecifier (_, None) ->
          "*"

    let gen (t: (_, _) Ast.Statement.ExportNamedDeclaration.t) =
      let declaration = match t.declaration with
        | Some st -> Statement.gen st
        | None -> ""
      in
      let specifiers = match t.specifiers with
        | Some s -> gen_specifier s
        | None -> ""
      in
      let source = match t.source with
        | Some (_, src) -> "from " ^ StringLiteral.gen src
        | None -> ""
      in
      let ws =
        if Option.(is_some t.specifiers && is_some t.source) then " " else "" in
      "export " ^ declaration ^ specifiers ^ ws ^ source
  end

  module ExportDefaultDeclaration = struct
    let gen (t: (_, _) Ast.Statement.ExportDefaultDeclaration.t) =
      let declaration = Statement.gen ~no_declare:true t.declaration in
      "export default " ^ declaration
  end

  module ImportDeclaration = struct
    let gen_named_specifier (s: _ Ast.Statement.ImportDeclaration.named_specifier) =
      let local = match s.local with
        | Some l -> " as " ^ Identifier.gen l
        | None -> ""
      in
      let remote = Identifier.gen s.remote in
      remote ^ local

    let gen_specifier (s: (_, _) Ast.Statement.ImportDeclaration.specifier) =
      match s with
      | ImportNamedSpecifiers xs ->
        let str = List.map xs ~f:gen_named_specifier |> String.concat ~sep:", " in
        "{ " ^ str ^ "}"
      | ImportNamespaceSpecifier (_, id) ->
        "* as " ^ Identifier.gen id

    let gen (t: (_, _) Ast.Statement.ImportDeclaration.t) =
      let (_, source) = t.source in
      let source = StringLiteral.gen source in
      let default = match t.default with
        | Some id -> Identifier.gen id
        | None -> ""
      in
      let specifiers = match t.specifiers with
        | Some s -> gen_specifier s
        | None -> ""
      in
      let comma =
        if String.(default <> "" && specifiers <> "")
        then ", " else ""
      in
      "import " ^ default ^ comma ^ specifiers ^ " from " ^ source
  end

  let gen ?(no_declare = false) ((_, t'): (_, _) Ast.Statement.t) =
    match t' with
    | DeclareClass s -> DeclareClass.gen ~no_declare s
    | DeclareFunction s -> DeclareFunction.gen ~no_declare s
    | DeclareInterface s ->
      let declare = if no_declare then "" else "declare " in
      declare ^ Interface.gen s
    | DeclareTypeAlias s ->
      let declare = if no_declare then "" else "declare " in
      declare ^ TypeAlias.gen s
    | DeclareVariable s -> DeclareVariable.gen s
    | ExportDefaultDeclaration s -> ExportDefaultDeclaration.gen s
    | ExportNamedDeclaration s -> ExportNamedDeclaration.gen s
    | ImportDeclaration s -> ImportDeclaration.gen s
    | InterfaceDeclaration s -> Interface.gen s
    | TypeAlias s -> TypeAlias.gen s
    | RawTs str -> str
end

(* without comments *)
(* let gen_program_simple ((_, stats, _comments): (_, _) Ast.program) =
  List.map stats ~f:Statement.gen |> String.concat ~sep:"\n" *)

let gen_program: (Loc.t, Loc.t) Ast.program -> string =
  let module Node = struct
    type t = {
      loc: Loc.t;
      gen: unit -> string;
    }
  end in
  let create_stat_node s = Node.{
    loc = fst s;
    gen = fun () -> Statement.gen s;
  } in
  let create_comment_node c = Node.{
    loc = fst c;
    gen = fun () -> Comment.gen c;
  } in
  let compare (a: Node.t) (b: Node.t) =
    Loc.compare_position a.loc.start b.loc.start in
  fun (_, stats, comments) ->
    let list = List.map stats ~f:create_stat_node
      @ List.map comments ~f:create_comment_node in
    let sorted = List.stable_sort list ~compare in
    let strings = List.map sorted ~f:(fun n -> n.gen ()) in
    String.concat ~sep:"\n" strings
