(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (import "P0lib" "writestr" (func $writestr (param i32)))
  (data (i32.const 65536) "\04\00\01\00")
  (func $quot (param $x i32) (param $y i32) (result i32)
    (local $q i32)
    (local $r i32)
    i32.const 0
    local.set $q
    local.get $x
    local.set $r
    loop
      local.get $r
      local.get $y
      i32.ge_s
      if
        local.get $r
        local.get $y
        i32.sub
        local.set $r
        local.get $q
        i32.const 1
        i32.add
        local.set $q
        br 1
      end
    end
    local.get $q
  )
  (func $program
    (local $x i32)
    (local $y i32)
    (local $q i32)
    (local $r i32)
    call $read
    local.set $x
    call $read
    local.set $y
    local.get $x
    local.get $y
    call $quot
    local.set $q
    local.get $q
    call $write
    call $writeln
  )
  (memory $memory 2)
  (export "memory" (memory $memory))
  (export "program" (func $program))
)
