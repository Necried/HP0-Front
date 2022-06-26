procedure fib(n: integer) → (res: integer)
    var f1, f2: integer
    if n = 0 or n = 1 then
	res := 1
    else
	f1 ← fib(n - 1)
	f2 ← fib(n - 2)
	res := f1 + f2

program fibmain
    var n, f: integer
    n ← read()
    f ← fib(n)
    write(f)
    writeln()
