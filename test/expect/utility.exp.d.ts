declare class Cl {  }
type O = { o: 0 }
type T = { t: 1 }
type U = { u: 2 }
type K = 'str'
type F = (() => void)
type ClT1 = typeof Cl
type ClT2 = typeof Cl
type Partial1 = Partial<T>
type ReadOnly1 = Readonly<T>
type ReadOnlyArray1 = ReadonlyArray<unknown>
type Keys1 = keyof T
type Values1 = T[keyof T]
type Exact1 = T
type PropertyType1 = O['key']
type ElementTyp1 = T[(K | 'key')]
type Return1 = ReturnType<F>
type NonMaybeType1 = NonNullable<(O | null | void)>
