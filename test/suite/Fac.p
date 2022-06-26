procedure fact(n: integer) → (f: integer)
    if n = 0 then
	f := 1
    else
	f ← fact(n - 1); f := f × n

program factorial
    var y, z: integer
    y ← read(); z ← fact(y); write(z)
