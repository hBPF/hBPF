# hBPF Design Doc

This document outlines the hBPF embedded domain-specific language for generating xt_bpf code for the purpose of writing
packet filters at a higher level than either BPF assembly or even pcap syntax allows. We leverage Haskell's clean
syntax for representing pure functions to create composable combinators that go through a compilation step to become a
series of xt_bpf instructions, which can then be compiled on your platform for a working filter.


hBPF Language
--------------

### Language Overview

The hBPF dialect is an embedded DSL that has a Haskell frontend. The upside of using Haskell in particular is that
the syntax is declarative, making it easy to right intuitive, type-checked BPF filters.

#### Examples

Here are just a few example filters, all built off of some simple combinators built off of the library itself:

```
-- Given a packet, returns true or false
httpGet :: IPPacket -> FilterCheck
httpGet pack = tcpPayload pack `first` 4 `mustEqual` "HTTP"
```

### Internals

hBPF has the following base types:

```
{-# LANGUAGE OverloadedStrings -#}
{-# Filters are defined like so #-}
class Filter where
  filter :: Packet -> Bool
  toBytecode :: [BPFByteCode]

-- Need to be able to turn a filter into a set of instructions
newtype BaseFilter = BaseFilter ()
instance Filter BaseFilter where
  filter p = True
  toBytecode = []

-- Set of combinators
```

It uses a simple interface that allows for the combination of several such filters into one larger filter.
