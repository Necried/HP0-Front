(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $ifThen4 (param $x i32) (result i32)
    (local $y i32)
    local.get $x
    i32.const 0
    i32.lt_s
    if
      i32.const 1
      local.set $y
    else
      local.get $y
      i32.const 1
      i32.sub
      call $ifThen4
      local.set $y
      local.get $y
      local.get $x
      i32.mul
      local.set $y
    end
    local.get $y
  )
  (func $ifThenProg
    (local $res i32)
    (local $res2 i32)
    i32.const 4
    call $ifThen4
    local.set $res
    local.get $res
    call $write
  )
  (memory 1)
  (start $ifThenProg)
)
