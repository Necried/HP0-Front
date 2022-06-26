(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $fact (param $n i32) (result i32)
    (local $f i32)
    local.get $n
    i32.const 0
    i32.eq
    if
      i32.const 1
      local.set $f
    else
      local.get $n
      i32.const 1
      i32.sub
      call $fact
      local.set $f
      local.get $f
      local.get $n
      i32.mul
      local.set $f
    end
    local.get $f
  )
  (func $factorial
    (local $y i32)
    (local $z i32)
    call $read
    local.set $y
    local.get $y
    call $fact
    local.set $z
    local.get $z
    call $write
  )
  (memory 1)
  (start $factorial)
)
