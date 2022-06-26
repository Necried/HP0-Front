type R = boolean
type S = 1..11 → R
type T = 3..9 → S
var x: T
var y: integer
var b: boolean
program p
    y := 3
    x[y][5] := false
    b := x[y][y + 1]
