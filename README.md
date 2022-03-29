# purescript-int64

Signed and unsigned 64-bit integer types.

## Design

From [dcodeIO/long.js](https://github.com/dcodeIO/long.js#webassembly-support):

> ## WebAssembly support
>
> [WebAssembly](http://webassembly.org) supports 64-bit integer arithmetic out
> of the box, hence a [tiny WebAssembly module](./src/Internal/wasm.wat) is
> used to compute operations like multiplication, division and remainder more
> efficiently (slow operations like division are around twice as fast), falling
> back to floating point based computations in JavaScript where WebAssembly is
> not yet supported, e.g., in older versions of node.

## Provenance

Most of this code was copied from

* [zapph/purescript-longs](https://github.com/zapph/purescript-longs) [LICENSE](./LICENSE.ZAPGroup)
* [dcodeIO/long.js](https://github.com/dcodeIO/long.js) [LICENSE](./LICENSE.dcodeIO)

