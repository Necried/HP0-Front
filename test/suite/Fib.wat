(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $fib (param $n i32) (result i32)
    (local $res i32)
    (local $f1 i32)
    (local $f2 i32)
    local.get $n
    i32.const 0
    i32.eq
    if (result i32)
      i32.const 1
    else
      local.get $n
      i32.const 1
      i32.eq
    end
    if
      i32.const 1
      local.set $res
    else
      local.get $n
      i32.const 1
      i32.sub
      call $fib
      local.set $f1
      local.get $n
      i32.const 2
      i32.sub
      call $fib
      local.set $f2
      local.get $f1
      local.get $f2
      i32.add
      local.set $res
    end
    local.get $res
  )
  (start $program)
  (func $program
    (local $n i32)
    (local $f i32)
    call $read
    local.set $n
    local.get $n
    call $fib
    local.set $f
    local.get $f
    call $write
    call $writeln
  )
  (memory 1)

)
