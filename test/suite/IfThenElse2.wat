(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $ifThen (param $x i32)
    i32.const 5
    local.set $x
    local.get $x
    i32.const 5
    i32.lt_s
    if
      local.get $x
      i32.const 2
      i32.add
      local.set $x
      local.get $x
      i32.const 1
      i32.add
      local.set $x
    else
      local.get $x
      i32.const 3
      i32.add
      local.set $x
    end
    local.get $x
    call $write
  )
  (func $ifThenProg
    (local $x i32)
    local.get $x
    call $ifThen
  )
  (memory 1)
  (start $ifThenProg)
)
