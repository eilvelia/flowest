type T1 = 1
export type T2 = 2
export type { T1 }
export type { T1 as T1A }
declare class Cl1 {}
declare export class Cl2 {}
// declare export { Cl1 } // TODO:
// declare export { Cl1 as Cl1A } // TODO:
export { Cl1 as Cl1B }
declare export default class Cl3 {}
declare export var v1: string
declare export function f1 (a: string): void
export * from './import'
export type * from './import'
export { T3 } from './import'
export { T4 as TT4 } from './import'
export type { T5, T6 } from './import'
export { T7, T8, T9 } from './import'
// declare export { U1, U2 } from './import' // TODO:
