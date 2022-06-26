{ Note: This requires support of 2 additional features:
    1. Polymorphic functional types
    2. Variable length arrays
    3. (and how do we work around this...) dependent types on array bounds!
    4. for-loops xD
}
procedure map(f: (<A> -> <B>), a: x..y -> <A>) -> (b: x..y -> <B>)
    var i : integer
    for i in x..y do
	b[i] <- f(a[i])

program map
    var a: 1..5 -> boolean
    var b: 1..5 -> boolean
    for i in 1..5 do
	a[i] := false
    b <- map(not, a)
    write(b)