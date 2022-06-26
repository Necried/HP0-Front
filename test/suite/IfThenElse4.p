procedure ifThen4(x: integer) → (y: integer)
    if x < 0 then
	y := 1
    else
	y ← ifThen4(y - 1); y := y × x

program ifThenProg
    var res, res2: integer
    res ← ifThen4(4)
    write(res)
