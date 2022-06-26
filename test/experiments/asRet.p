{ don't have to name anonoymous function types }
procedure addCurried(x: integer) -> (f: (integer -> integer))
  { lambda syntax - specify input types, output type should be inferred by the compiler }
  f := \n: integer -> x + n

program arithmetic
  var a: integer
  a <- addCurried(3)(2)
  write(a) { prints 5 }
