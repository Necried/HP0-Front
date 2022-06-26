procedure rem(x, y: integer) → (r: integer)
    var t: integer
    t := x
    while t ≥ y do
	t := t - y
    r := t

program arithmetic
    var x, y, r: integer
    x ← read(); y ← read()
    r ← rem(x, y)
    write(r)
    writeln()
