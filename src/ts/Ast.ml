open! Base

(* Most of this code is taken from Flow AST
  https://github.com/facebook/flow/blob/3df6deff41/src/parser/flow_ast.ml *)

module%gen rec Syntax: sig
  (* type ('M, 'internal) t = {
    leading: 'M Comment.t list;
    trailing: 'M Comment.t list;
    internal: 'internal
  } *)
  type 'M t = {
    leading: 'M Comment.t list;
    trailing: 'M Comment.t list;
  }
  [@@deriving show]
end = Syntax

and Comment: sig
  type 'M t = 'M * t'
  and t' =
    | Block of string
    | Line of string
  [@@deriving show]
end = Comment

and Identifier: sig
  type 'M t = 'M * 'M t'
  and 'M t' = {
    name: string;
    comments: 'M Syntax.t option
  }
  [@@deriving show]
end = Identifier

and Class: sig
  module Implements: sig
    type ('M, 'T) t = 'M * ('M, 'T) t'
    and ('M, 'T) t' = {
      id: 'T Identifier.t;
      targs: ('M, 'T) Type.ParameterInstantiation.t option;
    }
    [@@deriving show]
  end
end = Class

and PrivateName: sig
  type 'M t = 'M * 'M Identifier.t
  [@@deriving show]
end = PrivateName

and Literal: sig
  module RegExp: sig
    type t = {
      pattern: string;
      flags: string;
    }
    [@@deriving show]
  end

  (* Literals also carry along their raw value *)
  type 'M t = {
    value: value;
    raw: string;
    comments: 'M Syntax.t option;
  }
  and value =
    | String of string
    | Boolean of bool
    | Null
    | Number of float
    | RegExp of RegExp.t
  [@@deriving show]
end = Literal

and StringLiteral: sig
  type t = {
    value: string;
    raw: string;
  }
  [@@deriving show]
end = StringLiteral

and NumberLiteral: sig
  type t = {
    value: float;
    raw: string;
  }
  [@@deriving show]
end = NumberLiteral

and Type: sig
  module Function: sig
    module Param: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        (* name: 'T Identifier.t option; *)
        name: 'T Identifier.t;
        annot: ('M, 'T) Type.t;
        optional: bool;
      }
      [@@deriving show]
    end
    module RestParam: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        argument: ('M, 'T) Param.t
      }
      [@@deriving show]
    end
    module Params: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        params: ('M, 'T) Param.t list;
        rest: ('M, 'T) RestParam.t option;
      }
      [@@deriving show]
    end
    type ('M, 'T) t = {
      tparams: ('M, 'T) Type.ParameterDeclaration.t option;
      params: ('M, 'T) Params.t;
      return: ('M, 'T) Type.t;
    }
    [@@deriving show]
  end

  module Generic: sig
    (* examples of a 'Generic' type:
      - O.A
      - T<string>
      - T
      - M.T<string, number>
    *)
    module Identifier: sig
      type ('M, 'T) t =
        | Unqualified of 'T Identifier.t
        | Qualified of ('M, 'T) qualified
      and ('M, 'T) qualified = 'M * ('M, 'T) qualified'
      and ('M, 'T) qualified' = {
        qualification: ('M, 'T) t;
        id: 'T Identifier.t
      }
      [@@deriving show]
    end
    type ('M, 'T) t = {
      id: ('M, 'T) Identifier.t;
      targs: ('M, 'T) Type.ParameterInstantiation.t option;
    }
    [@@deriving show]
  end

  module Object: sig
    module Property: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        (* key: ('M, 'T) Expression.Object.Property.key; *)
        key: ('M, 'T) key;
        (* value: ('M, 'T) value; *)
        value: ('M, 'T) Type.t;
        optional: bool;
        static: bool;
        (* proto: bool; *)
        _method: bool;
        readonly: bool;
        (* variance: 'M Variance.t option; *)
      }
      (* and ('M, 'T) value =
        | Init of ('M, 'T) Type.t
        | Get of ('M * ('M, 'T) Function.t)
        | Set of ('M * ('M, 'T) Function.t) *)
      and ('M, 'T) key =
        | Literal of ('T * 'M Literal.t)
        | Identifier of 'T Identifier.t
        | PrivateName of 'M PrivateName.t
        (* | Computed of ('M, 'T) Expression.t *)
      [@@deriving show]
    end
    (* module SpreadProperty: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        argument: ('M, 'T) Type.t;
      }
      [@@deriving show]
    end *)
    module Indexer: sig
      type ('M, 'T) t' = {
        (* id: 'M Identifier.t option; *)
        id: 'M Identifier.t;
        key: ('M, 'T) Type.t;
        value: ('M, 'T) Type.t;
        static: bool;
        readonly: bool;
        (* variance: 'M Variance.t option; *)
      }
      and ('M, 'T) t = 'M * ('M, 'T) t'
      [@@deriving show]
    end
    module CallProperty: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        value: 'M * ('M, 'T) Function.t;
        static: bool;
      }
      [@@deriving show]
    end
    (* module InternalSlot: sig
      type ('M, 'T) t = 'M * ('M, 'T) t'
      and ('M, 'T) t' = {
        id: 'M Identifier.t;
        value: ('M, 'T) Type.t;
        optional: bool;
        static: bool;
        _method: bool;
      }
      [@@deriving show]
    end *)

    type ('M, 'T) t = {
      (* exact: bool;
      inexact: bool; *)
      properties: ('M, 'T) property list;
    }
    and ('M, 'T) property =
      | Property of ('M, 'T) Property.t
      (* | SpreadProperty of ('M, 'T) SpreadProperty.t *)
      | Indexer of ('M, 'T) Indexer.t
      | CallProperty of ('M, 'T) CallProperty.t
      (* | InternalSlot of ('M, 'T) InternalSlot.t *)
    [@@deriving show]
  end

  module ParameterDeclaration: sig
    module TypeParam: sig
      type ('M, 'T) t = 'T * ('M, 'T) t'
      and ('M, 'T) t' = {
        name: 'T Identifier.t;
        (* bound: ('M, 'T) Type.annotation_or_hint; *)
        bound: ('M, 'T) Type.t option;
        (* variance: 'M Variance.t option; *)
        default: ('M, 'T) Type.t option;
      }
      [@@deriving show]
    end
    type ('M, 'T) t = 'M * ('M, 'T) t'
    and ('M, 'T) t' = ('M, 'T) TypeParam.t list
    [@@deriving show]
  end

  module ParameterInstantiation: sig
    type ('M, 'T) t = 'M * ('M, 'T) t'
    and ('M, 'T) t' = ('M, 'T) Type.t list
    [@@deriving show]
  end

  type ('M, 'T) t = 'T * ('M, 'T) t'

  and ('M, 'T) t' =
    | Any
    (* | Mixed *)
    | Unknown
    (* | Empty *)
    | Never
    | Void
    | Undefined
    | Null
    | Number
    | String
    | Boolean
    (* | Nullable of ('M, 'T) t *)
    | Function of ('M, 'T) Function.t
    | Object of ('M, 'T) Object.t
    (* | Interface of ('M, 'T) Interface.t *)
    | Array of ('M, 'T) t
    | Generic of ('M, 'T) Generic.t
    | Union of ('M, 'T) t * ('M, 'T) t * ('M, 'T) t list
    | Intersection of ('M, 'T) t * ('M, 'T) t * ('M, 'T) t list
    | Typeof of ('M, 'T) t
    | Tuple of ('M, 'T) t list
    | StringLiteral of StringLiteral.t
    | NumberLiteral of NumberLiteral.t
    | BooleanLiteral of bool
    (* | Exists *)
    | Keyof of ('M, 'T) t
    | Unique of ('M, 'T) t
    | Readonly of ('M, 'T) t
    | IndexedAccess of ('M, 'T) t * ('M, 'T) t
    | InlineTs of string

  (* and ('M, 'T) annotation = 'M * ('M, 'T) t

  and ('M, 'T) annotation_or_hint =
    | Missing of 'T
    | Available of ('M, 'T) Type.annotation *)
  [@@deriving show]
end = Type

and Statement: sig
  module TypeAlias: sig
    type ('M, 'T) t = {
      id: 'T Identifier.t;
      tparams: ('M, 'T) Type.ParameterDeclaration.t option;
      right: ('M, 'T) Type.t;
    }
    [@@deriving show]
  end

  module Interface: sig
    type ('M, 'T) t = {
      id: 'T Identifier.t;
      tparams: ('M, 'T) Type.ParameterDeclaration.t option;
      extends: ('M * ('M, 'T) Type.Generic.t) list;
      body: 'M * ('M, 'T) Type.Object.t;
    }
    [@@deriving show]
  end

  module DeclareClass: sig
    type ('M, 'T) t = {
      id: 'T Identifier.t;
      tparams: ('M, 'T) Type.ParameterDeclaration.t option;
      body: 'M * ('M, 'T) Type.Object.t;
      extends: ('M * ('M, 'T) Type.Generic.t) option;
      (* mixins: ('M * ('M, 'T) Type.Generic.t) list; *)
      implements: ('M, 'T) Class.Implements.t list;
    }
    [@@deriving show]
  end

  module DeclareVariable: sig
    type ('M, 'T) t = {
      id: 'T Identifier.t;
      (* annot: ('M, 'T) Type.annotation_or_hint; *)
      annot: ('M, 'T) Type.t option;
    }
    [@@deriving show]
  end

  module DeclareFunction: sig
    type ('M, 'T) t = {
      id: 'M Identifier.t;
      (* annot: ('M, 'T) Type.annotation; *)
      (* annot: ('M, 'T) Type.t; *)
      annot: ('M, 'T) Type.Function.t;
      (* predicate: ('M, 'T) Type.Predicate.t option; *)
    }
    [@@deriving show]
  end

  module ExportNamedDeclaration: sig
    module ExportSpecifier: sig
      type 'M t = 'M * 'M t'
      and 'M t' = {
        local: 'M Identifier.t;
        exported: 'M Identifier.t option;
      }
      [@@deriving show]
    end

    type ('M, 'T) t = {
      declaration: ('M, 'T) Statement.t option;
      specifiers: 'M specifier option;
      source: ('M * StringLiteral.t) option;
    }
    and 'M specifier =
      | ExportSpecifiers of 'M ExportSpecifier.t list
      | ExportBatchSpecifier of 'M * 'M Identifier.t option
    [@@deriving show]
  end

  module ExportDefaultDeclaration: sig
    type ('M, 'T) t = {
      default: 'M;
      (* declaration: ('M, 'T) declaration; *)
      declaration: ('M, 'T) Statement.t;
    }
    (* and ('M, 'T) declaration =
      | Declaration of ('M, 'T) Statement.t
      | Expression of ('M, 'T) Expression.t *)
    [@@deriving show]
  end

  (* module ImportDeclaration: sig
    type importKind =
      | ImportType
      | ImportTypeof
      | ImportValue

    and ('M, 'T) specifier =
      | ImportNamedSpecifiers of 'T named_specifier list
      | ImportNamespaceSpecifier of ('M * 'M Identifier.t)

    and 'T named_specifier = {
      kind: importKind option;
      local: 'T Identifier.t option;
      remote: 'T Identifier.t;
    }

    and ('M, 'T) t = {
      importKind: importKind;
      source: ('M * StringLiteral.t);
      default: 'T Identifier.t option;
      specifiers: ('M, 'T) specifier option;
    }
    [@@deriving show]
  end *)

  module ImportDeclaration: sig
    type ('M, 'T) specifier =
      | ImportNamedSpecifiers of 'T named_specifier list
      | ImportNamespaceSpecifier of ('M * 'M Identifier.t)

    and 'T named_specifier = {
      local: 'T Identifier.t option;
      remote: 'T Identifier.t;
    }

    and ('M, 'T) t = {
      source: ('M * StringLiteral.t);
      default: 'T Identifier.t option;
      specifiers: ('M, 'T) specifier option;
    }
    [@@deriving show]
  end

  type ('M, 'T) t = 'M * ('M, 'T) t'
  and ('M, 'T) t' =
    | DeclareClass of ('M, 'T) DeclareClass.t
    (* | DeclareExportDeclaration of ('M, 'T) DeclareExportDeclaration.t *)
    | DeclareFunction of ('M, 'T) DeclareFunction.t
    | DeclareInterface of ('M, 'T) Interface.t
    (* | DeclareModule of ('M, 'T) DeclareModule.t *)
    (* | DeclareModuleExports of ('M, 'T) Type.annotation *)
    | DeclareTypeAlias of ('M, 'T) TypeAlias.t
    (* | DeclareOpaqueType of ('M, 'T) OpaqueType.t *)
    | DeclareVariable of ('M, 'T) DeclareVariable.t
    | ExportDefaultDeclaration of ('M, 'T) ExportDefaultDeclaration.t
    | ExportNamedDeclaration of ('M, 'T) ExportNamedDeclaration.t
    | ImportDeclaration of ('M, 'T) ImportDeclaration.t
    | InterfaceDeclaration of ('M, 'T) Interface.t
    | TypeAlias of ('M, 'T) TypeAlias.t
    (* | OpaqueType of ('M, 'T) OpaqueType.t *)
    (* | RawTs of string *) (* TODO: *)
  [@@deriving show]
end = Statement

type ('M, 'T) program = 'M * ('M, 'T) Statement.t list * 'M Comment.t list
[@@deriving show]

(* https://github.com/Microsoft/TypeScript/blob/master/lib/typescript.d.ts *)
