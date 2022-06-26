(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\00\01\00\04"  )
  (func $whileProg
    (local $x i32)
    i32.const 1
    call $write
    i32.const 1
    local.set $x
    loop
      local.get $x
      i32.const 5
      i32.lt_s
      if
        local.get $x
        call $write
        local.get $x
        i32.const 1
        i32.add
        local.set $x
        br 1
      end
    end
  )
  (memory 2)
  (start $whileProg)
)
