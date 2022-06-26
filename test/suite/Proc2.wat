(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (table 1 funcref)
  (elem (i32.const 0) $rem)
  (func $rem (param $x i32) (param $y i32) (result i32)
    (local $r i32)
    (local $t i32)
    local.get $x
    local.set $t
    loop
      local.get $t
      local.get $y
      i32.ge_s
      if
        local.get $t
        local.get $y
        i32.sub
        local.set $t
        br 1
      end
    end
    local.get $t
    local.set $r
    local.get $r
  )
  (func $program
    (local $x i32)
    (local $y i32)
    (local $r i32)
    call $read
    local.set $x
    call $read
    local.set $y
    local.get $x
    local.get $y
    call $rem
    local.set $r
    local.get $r
    call $write
    call $writeln
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
