{ don't have to name anonoymous function types }
procedure addCurried(x: integer) -> (f: (integer -> integer))
  f := n -> x + n

program arithmetic
  var f: integer -> integer
  var a, b: integer
  f <- addCurried(3)
  b <- f(2)
  write(a) { prints 5 }
