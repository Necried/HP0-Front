procedure add1(x : integer) → (r : integer)
    r := x + 1

procedure addToX(x : integer, d : (integer) → integer) → (a : integer)
    a ← d(x)

program arithmetic
    var x, a: integer
    x := 4
    a ← addToX(x, add1)
    write(a)
    writeln()
