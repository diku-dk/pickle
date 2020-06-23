# Futhark serialisation library [![CI](https://github.com/diku-dk/pickle/workflows/CI/badge.svg)](https://github.com/diku-dk/pickle/actions) [![Documentation](https://futhark-lang.org/pkgs/github.com/diku-dk/pickle/status.svg)](https://futhark-lang.org/pkgs/github.com/diku-dk/pickle/latest/)

This library provides generic serialisation capabilities to Futhark
programmers [1,2]. The library is built around the notion of picklers,
which are values that can be constructed from a number of
combinators. Picklers are typed with an index type and given a
pickler, it is possible to serialise and deserialise (to and from byte
streams) values of the index type.

## Installation

```
$ futhark pkg add github.com/diku-dk/pickle
$ futhark pkg sync
```

## Usage example

```
$ futhark repl
[0]> import "lib/github.com/diku-dk/pickle/pickle"
[1]> pickle.(pickle (array (pair i32 i32)) [(1,2),(3,4)])
[0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8,
 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 4u8]
[2]> pickle.(unpickle (array (pair i32 i32)) [0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 4u8])
[(1i32, 2i32), (3i32, 4i32)]
[3]>
```

## Literature

[1] Martin Elsman. Type-Specialized Serialization with Sharing. In
*Sixth Symposium on Trends in Functional Programming (TFP’05)*. Tallinn,
Estonia. September 2005.

[2] Andrew Kennedy. Pickler Combinators (Functional Pearl). In
*Journal of Functional Programming. Volume 14, Issue 6,
pp. 727-739*. November 2004.
