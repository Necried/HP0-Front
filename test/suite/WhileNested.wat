(module
  (import "P0lib" "write" (func $write (param i32)))
  (import "P0lib" "writeln" (func $writeln))
  (import "P0lib" "read" (func $read (result i32)))
  (func $whilenested
    (local $x i32)
    (local $y i32)
    (local $n i32)
    i32.const 1
    local.set $x
    i32.const 0
    local.set $n
    loop
      local.get $x
      i32.const 5
      i32.lt_s
      if
        local.get $n
        local.get $x
        i32.add
        local.set $n
        i32.const 1
        local.set $y
        loop
          local.get $y
          i32.const 5
          i32.lt_s
          if
            local.get $n
            local.get $y
            i32.add
            local.set $n
            local.get $y
            i32.const 1
            i32.add
            local.set $y
            br 1
          end
        end
        local.get $x
        i32.const 1
        i32.add
        local.set $x
        br 1
      end
    end
    local.get $n
    call $write
  )
  (memory 1)
  (start $whilenested)
)
