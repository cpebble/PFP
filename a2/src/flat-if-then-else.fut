-- Flattening If-Then-Else nested inside a map
-- ==
-- compiled input { [false,true,false,true]
--                  [3i64,4,2,1]
--                  [1i32,2,3,4,5,6,7,8,9,10]
-- }
-- output { [3i64,4,2,1] [2i32,4,6,5,6,7,8,16,18,11] }
-- compiled input { 
-- [true, false, true, false, true]
-- [2i64, 2, 3, 3, 2]
-- [2i32, 1, 2, 1, 3, 2, 1, 3, 2, 1, 2, 1]
-- }
-- output { [2, 2, 3, 3, 2i64] [3i32, 2, 4, 2, 4, 3, 2, 6, 4, 2, 3, 2] }
-- compiled random input { [100]bool [100]i64 [10000000]i32 }

let sgmscan 't [n] (op: t->t->t) (ne: t) (flg : [n]i64) (arr : [n]t) : [n]t =
  let flgs_vals =
    scan ( \ (f1, x1) (f2,x2) ->
            let f = f1 | f2 in
            if f2 != 0 then (f, x2)
            else (f, op x1 x2) )
         (0,ne) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t =
    scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)

let mkFlagArray 't [m]
            (aoa_shp: [m]i64) (zero: t)       --aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t  ) : []t =         --aoa_val=[1,1,1,1,1,1,1]
  let shp_rot = map (\i->if i==0 then 0       --shp_rot=[0,0,3,1,0,4,2]
                         else aoa_shp[i-1]
                    ) (iota m)
  let shp_scn = scan (+) 0 shp_rot            --shp_scn=[0,0,3,4,4,8,10]
  let aoa_len = shp_scn[m-1]+aoa_shp[m-1]     --aoa_len= 10
  let shp_ind = map2 (\shp ind ->             --shp_ind=
                       if shp==0 then -1      --  [-1,0,3,-1,4,8,-1]
                       else ind               --scatter
                     ) aoa_shp shp_scn        --   [0,0,0,0,0,0,0,0,0,0]
  in scatter (replicate aoa_len zero)         --   [-1,0,3,-1,4,8,-1]
             shp_ind aoa_val                  --   [1,1,1,1,1,1,1]
                                              -- res = [1,0,0,1,1,0,0,0,1,0]

let partition2 [n] 't (conds: [n]bool) (dummy: t) (arr: [n]t) : (i64, [n]t) =
  let tflgs = map (\ c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs

  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else -1
  let indsF = map (+lst) tmp

  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF

  let fltarr= scatter (replicate n dummy) inds arr
  in  (lst, fltarr)

let mkII2 [m] (shp: [m]i64) : []i32 =
    let F = mkFlagArray shp 0 (replicate m 1)
    in  sgmscan (+) 0 F (map (const 1) F) |> map (\x->x-1)

-- Weekly 2, Exercise 2:
-- The function bellow should be the flatten version of:
--     map (\b xs -> if b then map f xs
--                        else map g xs
--         ) bs xss
-- where:
--   `bs` is a 1d array of booleans
--   `xss` is a 2d irregular array of shape `S1_xss` and flat data `D_xss`;
--         the shape is an array of size `m` and the data is an array of size `n`
--   the result is a tuple representing an irregular array:
--       result's shape
--       result's flat data
-- Please take a look at the Rule (8) of Flattening
-- (slides 34 and 35 of L4-irreg-flattening.pdf)
-- and adapt the code from there.
--
-- The task is of course to replace the dummy implementation below
-- that just returns the input array with the code that performs the
-- flattening.
-- (You may of course use the helper functions provided in this file.)
--
-- Test input: [[1,2,3],[4,5,6,7], [8,9], [10]]
-- Test shape: [3, 4, 2, 1
-- Test bools: [F, T, F, T]
let flatIf [n][m] (f: i32 -> i32) (g: i32->i32) (bs: [m]bool) (S1_xss: [m]i64, D_xss: [n]i32) = --: ([]i64, []i32) =
  let (spl, iinds) = partition2 bs 0 (indices bs)
  let (s_xss_then, s_xss_else) = split spl (map (\ii -> S1_xss[ii]) iinds)
  
  -- Make a mask
  let fl_arr = mkFlagArray S1_xss 0 (replicate m 1) :> [n]i32 -- [1, 0, 0, 1, 0, 0, 0, 1, 0, 1]
  let fl_inds = scan (+) 0 fl_arr |> map (\x -> x-1)
  let fl_mask = map (\i -> bs[i]) fl_inds
  let res = map2 (\b x -> if b then f x else g x) fl_mask D_xss
  in (S1_xss, res)

  --let (brk, Dp_xss) = partition2 fl_mask 0 D_xss
  --let (D_xss_th, D_xss_el) = split brk Dp_xss

  --let (brk, Dp_xss) = partition2 fl_mask 0 D_xss
  --let (D_xss_th, D_xss_el) = split brk Dp_xss
  --let D_res_th = map f D_xss_th
  --let D_res_el = map g D_xss_el
  --in (D_xss_th, D_xss_el)



-- echo "[false,true,false,true] [3,4,2,1] [1,2,3,4,5,6,7,8,9,10]" | ./flat-if-then-else
let main [n][m] (bs: [m]bool) (S1_xss: [m]i64) (D_xss: [n]i32) =
    flatIf (\x->x+1i32) (\x->2i32*x) bs (S1_xss, D_xss)
