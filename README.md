# Serialisation of Futhark values [![Build Status](https://travis-ci.org/diku-dk/pickle.svg?branch=master)](https://travis-ci.org/diku-dk/pickle)

Serialisation and deserialisation for Futhark.

## Installation

```
$ futhark-pkg add github.com/diku-dk/pickle
$ futhark-pkg sync
```

## Usage example

```
$ futharki
[0]> import "lib/github.com/diku-dk/pickle/pickle"
[1]> pickle.(pickle (array (pair i32 i32)) [(1,2),(3,4)])
[0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8,
 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 4u8]
[2]> pickle.(unpickle (array (pair i32 i32)) [0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 8u8, 0u8, 0u8, 0u8, 1u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 3u8, 0u8, 0u8, 0u8, 4u8])
[(1i32, 2i32), (3i32, 4i32)]
[3]>
```
