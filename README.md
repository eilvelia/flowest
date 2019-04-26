# Flowest

Converts [Flow][] declarations to [TypeScript][].

It is focused to work with code specially written for Flowest, not some random Flow code.

[Flow]: https://flow.org/
[TypeScript]: https://www.typescriptlang.org/

## Installation

You can install js_of_ocaml version via npm:

```console
$ npm install -D flowest
# Or: $ npm install --global flowest
```

Flowest can also be compiled to native code.\
You can manually build it from the source code (it should work much faster).

### Usage

```console
$ flowest <input-file> <output-file>
# Or: $ npx flowest <input-file> <output-file>
```

## Features

It supports raw TypeScript via `/*$$ ... */`:

```javascript
type A = string
/*$$
type B = Partial<{ a: A, b: number }>
*/
/*::
type C = 3
*/
```

Output:

```typescript
type A = string
type B = Partial<{ a: A, b: number }>
type C = 3
```

`flowest-remove-next-line` and `flowest-remove-line`:

```javascript
/*$$ type D = Partial<{ a: A, b: number }> */
// flowest-remove-next-line
type D = $Shape<{ a: A, b: number }>
```

Output:

```typescript
type D = Partial<{ a: A, b: number }>
```

### Table

- ✅ - Done
- 🚩 - Not fully correct

| Status | Name       | Flow                                    | TypeScript |
|-------|-------------|-----------------------------------------|------------|
|   ✅  | Maybe       | `?T`                                    | `T \| null \| undefined` |
|   ✅  | Null        | `null`                                  | `null` |
|   ✅  | Mixed       | `mixed`                                 | `unknown` |
|   ✅  | Void        | `void`                                  | `void` 🚩 |
|       | BigInt      | `type A = 2n`                           | `type A = 2n` |
|   ✅  | Union       | `A | B | C`                             | `A | B | C` |
|   ✅  | Intersection | `A & B & C`                            | `A & B & C` |
|   ✅  | Typeof      | `typeof T`                              | `typeof T` |
|   ✅  | Tuples      | `[T, U]`                                | `[T, U]` |
|   ✅  | Functions   | `(A, B) => C`                           | `(a: A, b: B) => C` |
|   ✅  | Predicates  | `(A, B) => C %checks`                   | `(a: A, b: B) => C` 🚩 |
|   ✅  | Inexact types | `{ a: A, ... }` or `{ a: A }`         | `{ a: A }` 🚩 |
|   ✅  | Exact types | `{\| a: A \|}`                          | `{ a: A }` 🚩 |
|   ✅  | Existential types | `*`                               | `any` (not expressible) 🚩 |
|   ✅  | Indexers    | `{ [K]: T }`                            | `{ [key: K]: T }` |
|   ✅  | Bounds      | `<T: string>`                           | `<T extends string>` |
|   ✅  | Read-only fields | `interface A { +b: B }`            | `interface A { readonly b: B }` |
|   ✅  | Write-only fields | `interface A { -c: C }`           | `interface A { c: C }` 🚩 |
|       | Inline interfaces | `type T = interface { a: A }`     | - |
|       | Spread properties | `{ a: A, ...O }`                  | - |
|       | Internal slots | `{ [[call]]: T => U }`               | - |
|       | Generator   | -                                       | - |
|       | AsyncGenerator | -                                    | - |
|   ✅  | Partial     | `$Rest<T, {}>`                          | `Partial<T>` |
|       | $Shape      | `$Shape<T>`                             | - (not expressible) |
|   ✅  | $ReadOnly   | `$ReadOnly<T>`                          | `Readonly<T>` |
|   ✅  | $ReadOnlyArray | `$ReadOnlyArray<T>`                  | `ReadonlyArray<T>` |
|   ✅  | $Keys       | `$Keys<T>`                              | `keyof T` |
|   ✅  | $Values     | `$Values<T>`                            | `T[keyof T]` |
|   ✅  | $Exact      | `$Exact<T>`                             | `T` 🚩 |
|   ✅  | Class       | `Class<T>`                              | `typeof T` |
|   ✅  | Property type | `$PropertyType<O, k>`                 | `O[k]` |
|   ✅  | Element type | `$ElementType<T, K>`                   | `T[K]` |
|   ✅  | Return type | `$Call<F>`                              | `ReturnType<F>` |
|       | $Call       | `$Call<F, A1, A2, ..., Ak>`             | - (not expressible) |
|       | $Rest       | `$Rest<O1, O2>`                         | - |
|       | $Diff       | `$Diff<O1, O2>`                         | - |
|       | $ObjMap     | `$ObjMap<{ a: A }, <X>(X) => X>`        | - |
|       | $ObjMapi    | `$ObjMapi<{ a: A }, <I>(I) => I>`       | - |
|       | $TupleMap   | `$TupleMap<[1, 2, 3], <X>(X) => X>`     | - |
|   ✅  | $NonMaybeType | `$NonMaybeType<T>`                    | `NonNullable<T>` 🚩 |
|       | $CharSet    | `$CharSet<"abc">`                       | - (not expressible) |
|       | $Trusted    | `$Trusted<T>`                           | - (not expressible) |
|       | $Private    | `$Private<T>`                           | - (not expressible) |

#### Statements

| Status | Name       | Flow                                    | TypeScript |
|-------|-------------|-----------------------------------------|------------|
|   ✅  | Import default type | `import type T from './b'`      | `import T from './b'` |
|   ✅  | Import named type | `import type { T } from './b'`    | `import { T } from './b'` |
|   ✅  | Export type | `export type { T }`                     | `export { T }` |
|   ✅  | Declare export | `declare export class Cl {}`         | `export declare class Cl {}` |
|       | Declare export default | `declare export default string` | - |
|       | Declare module | `declare module 'm' { }`             | - |
|       | Declare module exports | `declare module.exports: T`  | - |
|   ✅  | Type alias  | `type T = string`                       | `type T = string` |
|   ✅  | Declare type alias  | `declare type T = string`       | `declare type T = string` |
|   ✅  | Interface   | `interface I extends A, B {}`           | `interface I extends A, B {}` |
|   ✅  | Declare interface | `declare interface I {}`          | `declare interface I {}` |
|       | Opaque type | `opaque type T = string`                | - |
|       | Declare opaque type | `declare opaque type T = string` | - |
|   ✅  | Declare variable | `declare var a: number`            | `declare var a: number` |
|   ✅  | Declare function | `declare function f(string): number` | `declare function f(a: string): number` |
|   ✅  | Declare class | `declare class B<T, U = D> extends A implements I1, I2 {}` | the same |
|       | `mixins` in declare class | `declare class B mixins A {}` | -

You can manually write TS code inside `/*$$ ... */` for a feature that is not supported.

---

Supported version of Flow parser: 0.96.1
