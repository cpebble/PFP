-- Test for exercise 1.1
-- #disabled for benching purposes ==
-- entry: process
-- compiled input @ exercise1.input output { 73 }

-- Test for exercise 1.2
-- ==
-- entry: test_process
-- random input {[100]i32 [100]i32}
-- random input {[1000]i32 [1000]i32}
-- random input {[10000]i32 [10000]i32}
-- random input {[100000]i32 [100000]i32}
-- random input {[1000000]i32 [1000000]i32}
-- random input {[10000000]i32 [10000000]i32}
-- random input {[100000000]i32 [100000000]i32}

-- create "process" function
-- Subtracts s1 from s2. makes all values positive and find the max value
entry process [n] (signal1: [n]i32) (signal2: [n]i32) =
  reduce i32.max 0 (map i32.abs (map2 (-) signal1 signal2))

entry test_process [n] (signal1: [n]i32) (signal2: [n]i32) =
  process signal1 signal2
