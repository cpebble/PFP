-- ==
-- entry: hillis_steele
-- random input {[4]i32}
-- random input {[16]i32}
-- random input {[256]i32}
-- random input {[1024]i32}
-- random input {[4096]i32}
-- random input {[16384]i32}

-- ==
-- entry: work_efficient
-- random input {[4]i32}
-- random input {[16]i32}
-- random input {[256]i32}
-- random input {[1024]i32}
-- random input {[4096]i32}
-- random input {[16384]i32}

let ilog2 (x: i64) = i64.i32 (63 - i64.clz x)
entry hillis_steele [n] (xs: [n]i32) =
  let m = ilog2 n
  in loop xs = copy xs for d in 0..<m do
    map2 (\x i -> 
      -- for each element x_i in xsn, is updated by xs[i] + xs[i-(2**d)]
      if i - (2**d) < 0 then x
      else (x + xs[i-(2**d)])
    ) xs (iota n) 


entry work_efficient [n] (xs: [n]i32) : [n]i32 = 
  let m = ilog2 n 
  let upswept = -- We want to go from m -> 0
    loop xs = copy xs for d in 1...m do
      map2 (\x i ->
        if (i+1) % (2**d) != 0 then x else
        x + xs[i-(2**(d-1))]
        ) xs (iota n)
  let upswept[n-1] = 0

  let downswept =
    loop xs = upswept for d in (m-1)..(m-2)...0 do
      map2 (\x i ->
        let k = (2**d) in
        if (i+1) % k != 0 then x -- Only operate on every k'th
        else
          -- Right parent
          if (i+1) % (k*2) != 0 then
            xs[i+k]
          else
          -- or left parent+direct parent
          x + xs[i-k]
        ) xs (iota n)

  in downswept
