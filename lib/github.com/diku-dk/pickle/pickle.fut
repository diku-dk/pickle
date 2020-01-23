-- Serialisation and deserialisation of Futhark values to byte
-- sequences ("pickling").

-- | This module contains picklers for primitive Futhark values, as
-- well as combinators for creating array- and tuple-picklers.  It can
-- be used directly, or as a building block for application-specific
-- picklers.  Trying to unpickle an invalid byte sequence may crash
-- the program.
module type pickle = {
  -- | A pickler that describes both how to pickle and unpickle a
  -- value of type `a`.
  type^ pu 'a

  -- | A sequence of bytes.
  type bytes [n] = [n]u8

  -- | Convert a value to a byte sequence.
  val pickle 'a : pu a -> a -> bytes []
  -- | Recover a value from a byte sequence.
  val unpickle 'a [n] : pu a -> bytes [n] -> a

  val i8 : pu i8
  val i16 : pu i16
  val i32 : pu i32
  val i64 : pu i64

  val u8 : pu u8
  val u16 : pu u16
  val u32 : pu u32
  val u64 : pu u64

  val f32 : pu f32
  val f64 : pu f64

  val bool : pu bool
  val pair 'a 'b : pu a -> pu b -> pu (a,b)
  val array 'a : pu a -> pu ([]a)

  -- | Given an isomorphism between types `a` and `b`, as well as a
  -- pickler for `a`, produce a pickler for `b`.  This is particularly
  -- handy for pickling records, as you can simply describe how they
  -- can be converted into nested pairs and back again, and then use
  -- the `pair`@term combinator.
  val iso 'a 'b : (a->b) -> (b->a) -> pu a -> pu b
}

module pickle : pickle = {
  type bytes [n] = [n]u8

  type^ pu 'a = { pickler : a -> bytes []
                , unpickler : (n: i32) -> bytes [n] -> (a, bytes [])
                }

  let pickle 'a (pu: pu a) = pu.pickler

  let unpickle 'a [n] (pu: pu a) (s: bytes [n]) =
    pu.unpickler n s |> (.0)

  let iso 'a 'b (f:a->b) (g:b->a) (pu:pu a) : pu b =
    { pickler = pu.pickler <-< g
    , unpickler = \n s -> let (v,s) = pu.unpickler n s
                          in (f v,s)
    }

  let i8 : pu i8 =
    { pickler = \x -> [u8.i8 x]
    , unpickler = \_ s -> (i8.u8 s[0],
                           s[1:])
    }

  let i16 : pu i16 =
    { pickler = \x -> [u8.i16 (x>>8),
                       u8.i16 (x>>0)]
    , unpickler = \_ s -> (i16.u8 s[0] << 8 |
                           i16.u8 s[1] << 0,
                           s[2:])
    }

  let i32 : pu i32 =
    { pickler = \x -> [u8.i32 (x>>24),
                       u8.i32 (x>>16),
                       u8.i32 (x>>8),
                       u8.i32 (x>>0)]
    , unpickler = \_ s -> (i32.u8 s[0] << 24 |
                           i32.u8 s[1] << 16 |
                           i32.u8 s[2] << 8 |
                           i32.u8 s[3] << 0,
                           s[4:])
    }

  let i64 : pu i64 =
    { pickler = \x -> [u8.i64 (x>>56),
                       u8.i64 (x>>48),
                       u8.i64 (x>>40),
                       u8.i64 (x>>32),
                       u8.i64 (x>>24),
                       u8.i64 (x>>16),
                       u8.i64 (x>>8),
                       u8.i64 (x>>0)]
    , unpickler = \_ s -> (i64.u8 s[0] << 56 |
                           i64.u8 s[1] << 48 |
                           i64.u8 s[2] << 40 |
                           i64.u8 s[3] << 32 |
                           i64.u8 s[4] << 24 |
                           i64.u8 s[5] << 16 |
                           i64.u8 s[6] << 8 |
                           i64.u8 s[7] << 0,
                           s[8:])
    }

  module math = import "/futlib/math"

  let u64 = iso math.u64.i64 math.i64.u64 i64
  let u32 = iso math.u32.i32 math.i32.u32 i32
  let u16 = iso math.u16.i16 math.i16.u16 i16
  let u8  = iso math.u8.i8   math.i8.u8   i8

  let f32 = iso f32.from_bits f32.to_bits u32
  let f64 = iso f64.from_bits f64.to_bits u64

  let bool : pu bool = iso math.bool.i8 math.i8.bool i8

  let pair 'a 'b (pu_a: pu a) (pu_b: pu b) =
    { pickler = \(a, b) -> pu_a.pickler a ++ pu_b.pickler b
    , unpickler = \n s -> let (a, s) = pu_a.unpickler n s
                          let (b, s) = pu_b.unpickler n s
                          in ((a, b), s)
    }

  let array 'a (pu: pu a) =
    { pickler = \arr ->
        let n = i32.pickler (length arr)
        let s = flatten (map pu.pickler arr)
        in n ++ i32.pickler (length s / length arr) ++ s
    , unpickler = \n s ->
        let (n, s) = i32.unpickler n s
        let (k, s) = i32.unpickler (length s) s
        let (arr_s, s) = split (n*k) s
        let arr = map (pu.unpickler k >-> (.0)) (unflatten n k arr_s)
        in (arr, s)
    }
}
