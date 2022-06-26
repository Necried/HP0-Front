(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\2d\00\01\00")
  (data (i32.const 65540) "\05\00\00\00\05\00\00\00\48\65\6c\6c\6f")
  (data (i32.const 65553) "\04\00\00\00\04\00\00\00\20\70\61\73")
  (data (i32.const 65565) "\08\00\00\00\08\00\00\00\78\64\21\21\21\21\21\21")
  (func $program
    (local $s i32)
    (local $t i32)
    (local $u i32)
    i32.const 65540
    local.set $s
    i32.const 65553
    local.set $t
    i32.const 65565
    local.set $u
    i32.const 4
    i32.const 4
    local.get $s
    i32.store
    i32.const 8
    local.get $t
    i32.store
    i32.const 65536
    i32.load
    i32.const 4
    i32.add
    i32.const 8
    i32.load
    i32.const 4
    i32.add
    i32.load
    i32.const 4
    i32.load
    i32.const 4
    i32.add
    i32.load
    i32.const 65536
    i32.load
    i32.const 8
    i32.load
    i32.load
    i32.const 4
    i32.load
    i32.load
    i32.const 65536
    i32.load
    i32.const 4
    i32.load
    i32.load
    i32.add
    i32.const 8
    i32.add
    i32.const 8
    i32.load
    i32.const 8
    i32.add
    i32.const 8
    i32.load
    i32.load
    i32.const 65536
    i32.load
    i32.const 8
    i32.add
    i32.const 4
    i32.load
    i32.const 8
    i32.add
    i32.const 4
    i32.load
    i32.load
    memory.copy
    memory.copy
    i32.add
    i32.store
    i32.add
    i32.store
    i32.const 65536
    i32.load
    i32.const 65536
    i32.const 65536
    i32.load
    i32.const 65536
    i32.load
    i32.load
    i32.add
    i32.const 8
    i32.add
    i32.store
    i32.store
    i32.const 8
    local.get $u
    i32.store
    i32.const 65536
    i32.load
    i32.const 4
    i32.add
    i32.const 8
    i32.load
    i32.const 4
    i32.add
    i32.load
    i32.const 4
    i32.load
    i32.const 4
    i32.add
    i32.load
    i32.const 65536
    i32.load
    i32.const 8
    i32.load
    i32.load
    i32.const 4
    i32.load
    i32.load
    i32.const 65536
    i32.load
    i32.const 4
    i32.load
    i32.load
    i32.add
    i32.const 8
    i32.add
    i32.const 8
    i32.load
    i32.const 8
    i32.add
    i32.const 8
    i32.load
    i32.load
    i32.const 65536
    i32.load
    i32.const 8
    i32.add
    i32.const 4
    i32.load
    i32.const 8
    i32.add
    i32.const 4
    i32.load
    i32.load
    memory.copy
    memory.copy
    i32.add
    i32.store
    i32.add
    i32.store
    i32.const 65536
    i32.load
    i32.const 65536
    i32.const 65536
    i32.load
    i32.const 65536
    i32.load
    i32.load
    i32.add
    i32.const 8
    i32.add
    i32.store
    call $writestr
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
