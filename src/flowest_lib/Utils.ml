open! Base

let first (a, b) ~f = (f a, b)

let second (a, b) ~f = (a, f b)
