(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (table 0 funcref)
  (elem (i32.const 0) )
  (func $program
    i32.const 1
    if
      i32.const 10
      call $write
    else
      i32.const 20
      call $write
    end
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
