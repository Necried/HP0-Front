(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (table 2 funcref)
  (elem (i32.const 0) $add1 $addToX)
  (type $addToX_5199267747263203230 (func (param i32) (result i32)))
  (func $add1 (param $x i32) (result i32)
    (local $r i32)
    local.get $x
    i32.const 1
    i32.add
    local.set $r
    local.get $r
  )
  (func $addToX (param $x i32) (param $d i32) (result i32)
    (local $a i32)
    local.get $x
    (call_indirect (type $addToX_5199267747263203230) (local.get $d))
    local.set $a
    local.get $a
  )
  (func $program
    (local $x i32)
    (local $a i32)
    i32.const 4
    local.set $x
    local.get $x
    i32.const 0
    call $addToX
    local.set $a
    local.get $a
    call $write
    call $writeln
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
