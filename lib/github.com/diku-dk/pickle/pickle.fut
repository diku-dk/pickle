module type pickle = {
  type^ pu 'a

  type bytes = []u8

  val pickle 'a : pu a -> a -> bytes
  val unpickle 'a : pu a -> bytes -> a

  val i8 : pu i8
  val i16 : pu i16
  val i32 : pu i32
  val i64 : pu i64

  val u8 : pu u8
  val u16 : pu u16
  val u32 : pu u32
  val u64 : pu u64

  val bool : pu bool
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

  let i64 = { pickler = \x: bytes -> [u8.i64 (x>>56),
                                      u8.i64 (x>>48),
                                      u8.i64 (x>>40),
                                      u8.i64 (x>>32),
                                      u8.i64 (x>>24),
                                      u8.i64 (x>>16),
                                      u8.i64 (x>>8),
                                      u8.i64 (x>>0)]
            , unpickler = \s -> (i64.u8 s[0] << 56 |
                                 i64.u8 s[1] << 48 |
                                 i64.u8 s[2] << 40 |
                                 i64.u8 s[3] << 32 |
                                 i64.u8 s[4] << 24 |
                                 i64.u8 s[5] << 16 |
                                 i64.u8 s[6] << 8 |
                                 i64.u8 s[7] << 0,
                                 s[8:])
            }

  let u64 = { pickler = \x: bytes -> [u8.u64 (x>>56),
                                      u8.u64 (x>>48),
                                      u8.u64 (x>>40),
                                      u8.u64 (x>>32),
                                      u8.u64 (x>>24),
                                      u8.u64 (x>>16),
                                      u8.u64 (x>>8),
                                      u8.u64 (x>>0)]
            , unpickler = \s -> (u64.u8 s[0] << 56 |
                                 u64.u8 s[1] << 48 |
                                 u64.u8 s[2] << 40 |
                                 u64.u8 s[3] << 32 |
                                 u64.u8 s[4] << 24 |
                                 u64.u8 s[5] << 16 |
                                 u64.u8 s[6] << 8 |
                                 u64.u8 s[7] << 0,
                                 s[8:])
            }

  let u32 = { pickler = \x: bytes -> [u8.u32 (x>>24),
                                      u8.u32 (x>>16),
                                      u8.u32 (x>>8),
                                      u8.u32 (x>>0)]
            , unpickler = \s -> (u32.u8 s[0] << 24 |
                                 u32.u8 s[1] << 16 |
                                 u32.u8 s[2] << 8 |
                                 u32.u8 s[3] << 0,
                                 s[4:])
            }

  let u16 = { pickler = \x: bytes -> [u8.u16 (x>>8),
                                      u8.u16 (x>>0)]
            , unpickler = \s -> (u16.u8 s[0] << 8 |
                                 u16.u8 s[1] << 0,
                                 s[2:])
            }

  let u8 = { pickler = \x: bytes -> [x]
           , unpickler = \s -> (s[0],
                                s[1:])
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

  let bool : pu bool =
    iso (== 1i8) (\x -> if x then 1i8 else 0i8) i8
}
