(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $ifThen  (result i32)
    (local $y i32)
    local.get $y
    i32.const 5
    i32.lt_s
    if
      local.get $y
      i32.const 2
      i32.add
      local.set $y
    else
      local.get $y
      i32.const 3
      i32.add
      local.set $y
    end
    local.get $y
  )
  (func $ifThenProg
    (local $y i32)
    call $ifThen
    local.set $y
    local.get $y
    call $write
  )
  (memory 1)
  (start $ifThenProg)
)
