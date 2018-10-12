import "pickle"

-- ==
-- entry: test_i32
-- input { 3 } output { true }
-- input { -1 } output { true }
-- input { 0 } output { true }

-- ==
-- entry: many
-- input { 0 } output { true }

open pickle

let test pu eq v =
  eq(unpickle pu (pickle pu v)) v

entry test_i32 (x:i32) : bool =
  test i32 (==) x

entry many (_:i32) : bool =
  let pu = array (array (pair i32 (pair i8 i16)))
  let pu2 = iso (\(a,b) -> {a,b}) (\{a,b} -> (a,b)) (pair i32 i8)
  let v = [[(3,(1,2)),(6,(3,7))],
           [(4,(1,1)),(6,(4,2))]]
  let v2 = {a=23,b=12}
  in test pu (==) v && test pu2 (==) v2
