program whilenested
    var x, y, n: integer
    x := 1
    n := 0
    while x < 5 do
	n := n + x
	y := 1
	while y < 5 do
	    n := n + y
	    y := y + 1
	x := x + 1
    write(n)