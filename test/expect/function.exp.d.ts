declare function f(a: number, b: string): string
declare function g(nn: number, b: string): unknown
declare function h(a: number, b: string, c: string, d: 1, e: 2, f: 3, g: 4, h: 5): void
declare function opt(a?: number, b: number, c?: string): void
declare function typeparams1<A>(a: A): A
declare function typeparams2<A, B>(a: A): B
declare function typeparams3<A extends string, B>(a: A): B
type Arrow1 = ((a: string) => number)
type Arrow2 = ((a: A, b: B) => (A & B))
type Arrow3 = ((a: A, b: B) => (A & ((a: B) => C)))
type Arrow4 = ((a: A, b: B) => ((a: (A & B)) => C))
type Arrow5 = ((a: string, b?: number) => number)
type Arrow6 = (() => void)
declare function checks1(a: unknown): boolean
