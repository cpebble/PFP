-- Test for exercise 1.3
-- ==
-- entry: process_idx
-- random input {[100]i64 [100]i64}
-- random input {[1000]i64 [1000]i64}
-- random input {[10000]i64 [10000]i64}
-- random input {[100000]i64 [100000]i64}
-- random input {[1000000]i64 [1000000]i64}
-- random input {[10000000]i64 [10000000]i64}
-- random input {[100000000]i64 [100000000]i64}

-- create "process" function
-- Subtracts s1 from s2. makes all values positive and find the max value
entry process_idx [n] (signal1: [n]i64) (signal2: [n]i64) =
  let max2 (i1, v1) (i2, v2) = 
    if v1 > v2 then (i1, v1)
    else if v2 > v1 then (i2, v2)
    else if i1 < i2 then (i2, v2)
    else (i1, v1) in
  reduce max2 (-1, -1) (zip (iota n)
                            (map i64.abs (map2 (-) signal1 signal2)))

