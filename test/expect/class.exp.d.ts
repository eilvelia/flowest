declare class C1<T, U = D> extends A implements I1, I2 { constructor(a: string) }
declare class C2 { f: ((a: string) => number); constructor(a: string, b: number); m1(): void; m2(a: string): number; m3(): this }
declare class C3<A extends string, B extends string = "str"> extends C1<A> {  }
