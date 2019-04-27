declare class C1<T, U = D> extends A implements I1, I2 {
  constructor(string): this
}
declare class C2 {
  f: string => number;
  constructor(a: string, number): void;
  m1(): void;
  m2(string): number;
  m3(): this;
}
declare class C3<A: string, B: string = "str"> extends C1<A> {}
