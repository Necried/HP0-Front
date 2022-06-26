procedure isMultipleOf(n : integer)(i : integer) -> b : boolean
    b := i % n == 0

program isDouble
    isEven : integer -> integer
    isEven := isMultipleOf(2)
    write(isEven(4))
