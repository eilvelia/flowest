interface I1 { a: 1; readonly b: 2; c: 3; d?: 4; readonly e?: 5; f?: 6 }
interface I2 {  }
interface I3<A extends string = never> extends I1 {  }
interface I4<A, B, C = string> extends I3<B> { g: 7 }
interface I5<A> extends I1, I2, I3<never> { hi: 'hi' }
declare interface I6 extends I5<string> { h: 'h' }
