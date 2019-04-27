type T1 = 1
export type T2 = 2
export { T1 }
export { T1 as T1A }
declare class Cl1 {  }
export declare class Cl2 {  }
// declare export { Cl1 } // TODO:
// declare export { Cl1 as Cl1A } // TODO:
export { Cl1 as Cl1B }
export default class Cl3 {  }
export declare var v1: string
export declare function f1(a: string): void
export * from './import'
export * from './import'
export { T3 } from './import'
export { T4 as TT4 } from './import'
export { T5, T6 } from './import'
export { T7, T8, T9 } from './import'
// declare export { U1, U2 } from './import' // TODO:
