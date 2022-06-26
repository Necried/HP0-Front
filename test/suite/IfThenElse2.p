procedure ifThen(x: integer)
    x := 5
    if x < 5 then
	x := x + 2
    else
	x := x + 3
    write(x)

program ifThenProg
    var x: integer
    ifThen(x)
