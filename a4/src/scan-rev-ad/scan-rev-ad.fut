type real = f64
let real_abs = f64.abs
let real_max = f64.max

--
let nearlyEqual r1 r2 =
  let d = real_abs (r1-r2)
  let err = d / (real_max (real_abs r1) (real_abs r2))
  in  err < 0.0000000001

-- this function computes $d (x*y)/d x$
let df2dx (_: real) (y: real) = y

-- this function computes $d (x*y)/d y$
let df2dy (x: real) (_: real) = x

-- operator for linear function composition
let lin_o (a1: real, b1: real) (a2:real, b2: real) = (a2 + b2*a1, b1*b2)

-- from futhark-lang
-- let reverse [n] 't (xs: [n]t): [n]t = xs[::-1]

-- Weekly 4, Task 2: 
-- Implement the reverse-AD adjoint (differentiated) code  for scan (*) 0.0
--   (the current implementation is obviously a dummy one).
-- The high-level implementation is presented in the slides of Lecture 8:
--   the original `scan` code needs to be re-executed. Then you need to
--   implement the three steps that compute `cs`, `ds`, the scan with
--   linear-function composition as operator (stored in `ys_bar`), and
--   the final `map` that computes `as_bar`.
-- Input:
--   `as`: is the array which is the target of the `scan (*) 1.0` operation
--              in the original code (primal trace). 
--   `ys_bar_0`: is the adjoint of the result of scan, as computed by
--               the adjoint code until it reached (backwards) the point
--               where the `scan` was called.
-- Result:
--   The adjoint array of `as`. We assume `as_bar_0` contains only zero
--   elements, i.e., `as` has only been used by the `scan` operation. 
let scanAdjoint [n] (as: [n]real) (ys_bar_0: [n]real) : [n]real =
  let ys = scan (*) 1.0 as
  let is = iota n
  let cs = map (\i -> if i == n-1 then 1 else as[i+1]) is
  let ds = map (\i -> if i == n-1 then 0 else ys_bar_0[i]) is
  let ys_bar = scan lin_o (0, 1) (zip (reverse ds) (reverse cs))
    |> map (\ (d, c) -> d + (c * ys_bar_0[n-1]) )
    |> reverse
  in map (\i -> if i == 0 then ys_bar[0] else (ys[i-1]*ys_bar[i])) is


-- Implementation for the adjoint of `reduce (*) 1.0`,
-- which comforms with the slides.
let reduceAdjoint [n] (as: [n]real) : [n]real =
  let y = reduce (*) 1.0 as
  let y_bar = 1.0
  let as_bar = map (\a -> (y / a) * y_bar) as
  in  as_bar

-- The main function computes the Jacobian of the function that takes the
--   product of the array elements in two ways:
-- 1. By computing the derivative of `reduce (*) 1.0`, implemented by the
--    `reduceAdjoint` function
-- 2. By computing the adjoint of the `scan (*) 1.0`, implemented by the
--    `scanAdjoint` function, which is called with `ys_bar_0` equal to
--    the direction vector corresponding to output `n-1`,
--    i.e., `[0,...,0,1]`
--
-- The program returns `valid=true` if the relative error between the
--   corresponding elements of the resulted arrays is in all cases under
--   `0.0000000001`, see the implementation of `nearlyEqual`
--
-- Run it with:
--    $ futhark dataset --f64-bounds=0.5:1.6043 -g [1000000]f64 -b | ./scan-rev-ad -r 10 -t /dev/stderr
-- or with sizes smaller than `1000000`
--
let main [n] (as: [n]real) =
  let res1 = reduceAdjoint as

  -- We compute the last row of the Jacobian for scan, i.e., for
  -- output n-1; this should be equivalent with the reduce
  let ys_bar_0 = map (\i -> if i==(n-1) then 1.0 else 0.0) (iota n)
  let res2 = scanAdjoint as ys_bar_0

  -- do the two methods give the same result?
  let valid= map2 nearlyEqual res1 res2
          |> reduce_comm (&&) true
  in valid
    
