# Int64

__*WIP*__ I intend to move this package to https://github.com/purescript-contrib
after the PureScript 0.15 release stabilizes.

[![CI](https://github.com/purescript-contrib/purescript-int64/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-int64/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-int64.svg)](https://github.com/purescript-contrib/purescript-int64/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-int64/badge)](https://pursuit.purescript.org/packages/purescript-int64)
[![Maintainer: jamesdbrock](https://img.shields.io/badge/maintainer-jamesdbrock-teal.svg)](https://github.com/jamesdbrock)

Signed and unsigned 64-bit integer types and operations.

## Design

The arithmetic operations in this library will be performed by WebAssembly
functions for speed, if the environment allows.

From [__dcodeIO/long.js__](https://github.com/dcodeIO/long.js):

> A Long class for representing a 64 bit two's-complement integer value derived
> from the [Closure Library](https://github.com/google/closure-library)
> for stand-alone use and extended with unsigned support.

> ## WebAssembly support
>
> [WebAssembly](http://webassembly.org) supports 64-bit integer arithmetic out
> of the box, hence a [tiny WebAssembly module](./src/Data/Internal/long.js/wasm.wat) is
> used to compute operations like multiplication, division and remainder more
> efficiently (slow operations like division are around twice as fast), falling
> back to floating point based computations in JavaScript where WebAssembly is
> not yet supported, e.g., in older versions of node.

## Installation

Install `int64` with [Spago](https://github.com/purescript/spago):

```sh
spago install int64
```

## Quick start

## Documentation

`int64` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-int64).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-int64/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://discord.com/invite/sMqwYUbvz6) chat.

## Contributing

You can contribute to `int64` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-int64/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

## Development

Run the test suite:

    spago -x spago-dev.dhall test

## Provenance

Most of this code was copied from

* [__zapph/purescript-longs__](https://github.com/zapph/purescript-longs) [LICENSE](./LICENSE.ZAPGroup)
* [__dcodeIO/long.js__](https://github.com/dcodeIO/long.js) [LICENSE](./LICENSE.dcodeIO)

This should be considered the successor package to the package
__purescript-longs__, for which maintaince slowed down in 2019.
The module names and type names have been reorganized, and the __long.js__
dependency has been inlined instead of declared in
the `dependencies` of a `package.json`.

