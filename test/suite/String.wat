(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\17\00\01\00")
  (data (i32.const 65540) "\0b\00\00\00\08\00\00\00\48\65\6c\6c\6f\21\20\f0\9f\99\82")
  (func $program
    (local $s i32)
    (local $n i32)
    i32.const 65540
    local.set $s
    local.get $s
    call $writestr
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
