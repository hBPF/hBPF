hBPF: An Embedded Haskell DSL for BPF Assembly
----------------------------------------------

**hBPF** is a declarative DSL that is embedded in Haskell for forming and compiling BPF assembly.
Users write their filters in a high-level, typed functional syntax that then gets compiled under the hood into BPF assembly code.

The assembly code generated can then be passed into a BPF assembler, an example is [bpf_asm](https://github.com/torvalds/linux/blob/master/tools/bpf/bpf_asm.c) as provided under the tools/net directory in the Linux kernel.

This repository serves as the source code for the final project for Stanford's [CS 240H](http://www.scs.stanford.edu/16wi-cs240h/) taught by [David Mazières](http://www.scs.stanford.edu/~dm/) and [Bryan O'Sullivan](http://www.serpentine.com/blog/) in the Winter of 2016.

Work of Andrew Duffy (agd@cs.stanford.edu) and Matt Denton (mdenton@cs.stanford.edu)

