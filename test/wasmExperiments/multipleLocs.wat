(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65540) "\00\00\00\0b\00\00\00\08\48\65\6c\6c\6f\21\20\f0\9f\99\82")
  (func $string
    (local $s i32)
    i32.const 65540
    local.set $s
    local.get $s
    call $writestr
  )
  (memory 2)
  (start $string)
)
