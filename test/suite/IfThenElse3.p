procedure ifThen() → (y: integer)
    if y < 5 then
	y := y + 2
    else
	y := y + 3

program ifThenProg
    var y: integer
    y ← ifThen()
    write(y)