procedure quot(x, y: integer) → (q: integer)
    var r: integer
    q := 0; r := x
    while r ≥ y do
	r := r - y; q := q + 1

program arithmetic
    var x, y, q, r: integer
    x ← read(); y ← read()
    q ← quot(x, y)
    write(q)
    writeln()
