declare class Cl {}
type O = { o: 0 }
type T = { t: 1 }
type U = { u: 2 }
type K = 'str'
type F = () => void
type ClT1 = typeof Cl
type ClT2 = Class<Cl>
type Partial1 = $Rest<T, {}>
type ReadOnly1 = $ReadOnly<T>
type ReadOnlyArray1 = $ReadOnlyArray<mixed>
type Keys1 = $Keys<T>
type Values1 = $Values<T>
type Exact1 = $Exact<T>
type PropertyType1 = $PropertyType<O, 'key'>
type ElementTyp1 = $ElementType<T, K | 'key'>
type Return1 = $Call<F>
type NonMaybeType1 = $NonMaybeType<O | null | void>
