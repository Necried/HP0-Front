type T = 1..10 → integer
var a: T
procedure q(b: integer, c: integer)
    write(b); write(c)
procedure r() → (d: integer)
    a[3] := 9; d := 5
program p
    var x: integer
    a[2] := 7; q(3, a[2]) {writes 3, 7}
    x ← r(); write(x); write(a[3]) {writes 5, 9}
