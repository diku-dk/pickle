module type pickle = {
  type^ pu 'a

  type bytes = []u8

  val pickle 'a : pu a -> a -> bytes
  val unpickle 'a : pu a -> bytes -> a

  val i8 : pu i8
  val i16 : pu i16
  val i32 : pu i32
  val pair 'a 'b : pu a -> pu b -> pu (a,b)
  val array 'a : pu a -> pu ([]a)
  val iso 'a 'b : (a->b) -> (b->a) -> pu a -> pu b
}

module pickle : pickle = {
  type bytes = []u8

  type pu 'a = { pickler : a -> bytes
               , unpickler : bytes -> (a, bytes)
               }

  let pickle 'a (pu: pu a) = pu.pickler

  let unpickle 'a (pu: pu a) = pu.unpickler >-> (.1)

  let i8 = { pickler = \x: bytes -> [u8.i8 x]
           , unpickler = \s -> (i8.u8 s[0],
                                s[1:])
           }

  let i16 = { pickler = \x: bytes -> [u8.i16 (x>>8),
                                      u8.i16 (x>>0)]
            , unpickler = \s -> (i16.u8 s[0] << 8 |
                                 i16.u8 s[1] << 0,
                                 s[2:])
            }

  let i32 = { pickler = \x: bytes -> [u8.i32 (x>>24),
                                      u8.i32 (x>>16),
                                      u8.i32 (x>>8),
                                      u8.i32 (x>>0)]
            , unpickler = \s -> (i32.u8 s[0] << 24 |
                                 i32.u8 s[1] << 16 |
                                 i32.u8 s[2] << 8 |
                                 i32.u8 s[3] << 0,
                                 s[4:])
            }

  let pair 'a 'b (pu_a: pu a) (pu_b: pu b) =
    { pickler = \(a, b): bytes -> pu_a.pickler a ++ pu_b.pickler b
    , unpickler = \s -> let (a, s) = pu_a.unpickler s
                        let (b, s) = pu_b.unpickler s
                        in ((a, b), s)
    }

  let array 'a (pu: pu a) =
    { pickler = \arr: bytes ->
        let n = i32.pickler (length arr)
        let s = flatten (map pu.pickler arr)
        in n ++ i32.pickler (length s / length arr) ++ s
    , unpickler = \s ->
        let (n, s) = i32.unpickler s
        let (k, s) = i32.unpickler s
        let (arr_s, s) = split (n*k) s
        let arr = map (pu.unpickler >-> (.1)) (unflatten n k arr_s)
        in (arr, s)
    }

  let iso 'a 'b (f:a->b) (g:b->a) (pu:pu a) : pu b =
    { pickler = pu.pickler <-< g
    , unpickler = \s -> let (v,s) = pu.unpickler s
                        in (f v,s)
    }
}
