declare function f(a: number, b: string): string
declare function g(nn: number, string): mixed
declare function h(number, string, string, 1, 2, 3, 4, 5): void
declare function opt(a?: number, b: number, c?: string): void
declare function typeparams1<A>(A): A
declare function typeparams2<A, B>(a: A): B
declare function typeparams3<A: string, B>(a: A): B
type Arrow1 = string => number
type Arrow2 = (A, B) => A & B
type Arrow3 = (A, B) => A & B => C
type Arrow4 = (A, B) => (A & B) => C
type Arrow5 = (a: string, b?: number) => number
type Arrow6 = () => void
declare function checks1(a: mixed): boolean %checks(typeof a === 'string')
