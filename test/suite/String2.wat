(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (func $program
    (local $n i32)
    i32.const 1
    local.set $n
    local.get $n
    call $write
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
