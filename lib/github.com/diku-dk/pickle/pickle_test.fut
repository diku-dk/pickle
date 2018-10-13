import "pickle"
import "/futlib/math"

-- ==
-- entry: test_i32
-- input { 3 } output { true }
-- input { -1 } output { true }
-- input { 0 } output { true }

-- ==
-- entry: many
-- input { 0 } output { true }

module P = pickle

let test pu p v =
  p(P.unpickle pu (P.pickle pu v))

entry test_i32 (x:i32) : bool =
  test P.i32 (x ==) x

entry many (_:i32) : bool =
  let pu = P.array (P.array (P.pair P.i32 (P.pair P.i8 P.i16)))
  let pu2 = P.iso (\(a,b) -> {a,b}) (\{a,b} -> (a,b)) (P.pair P.i32 P.i8)
  let v = [[(3,(1,2)),(6,(3,7))],
           [(4,(1,1)),(6,(4,2))]]
  let v2 = {a=23,b=12}
  in test pu (v ==) v && test pu2 (v2 ==) v2
     && test P.i64 (0xFFFFFFFFFFFFFFF ==) 0xFFFFFFFFFFFFFFF
     && test P.i32 (\v -> i64.i32 v == 0x7FFFFFFF) 0x7FFFFFFF
     && test P.i16 (\v -> i64.i16 v == 0x7FFF) 0x7FFF
     && test P.i16 (\v -> i64.i16 v != 0x8000) 0x8000
     && test P.i8 (100 ==) 100
