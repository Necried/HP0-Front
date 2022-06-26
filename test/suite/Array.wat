(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (table 2 funcref)
  (elem (i32.const 0) $q $r)
  (func $q (param $b i32) (param $c i32)
    local.get $b
    call $write
    local.get $c
    call $write
  )
  (func $r  (result i32)
    (local $d i32)
    i32.const 3
    i32.const 1
    i32.sub
    i32.const 4
    i32.mul
    i32.const 12
    i32.add
    i32.const 9
    i32.store
    i32.const 5
    local.set $d
    local.get $d
  )
  (func $program
    (local $x i32)
    i32.const 2
    i32.const 1
    i32.sub
    i32.const 4
    i32.mul
    i32.const 12
    i32.add
    i32.const 7
    i32.store
    i32.const 3
    i32.const 2
    i32.const 1
    i32.sub
    i32.const 4
    i32.mul
    i32.const 12
    i32.add
    i32.load
    call $q
    call $r
    local.set $x
    local.get $x
    call $write
    i32.const 3
    i32.const 1
    i32.sub
    i32.const 4
    i32.mul
    i32.const 12
    i32.add
    i32.load
    call $write
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
