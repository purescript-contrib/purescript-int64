__*WIP*__ I intend to move this package to __purescript-contrib__ after the PureScript
0.15 release stabilizes.

# purescript-int64

Signed and unsigned 64-bit integer types.

## Design

From [dcodeIO/long.js](https://github.com/dcodeIO/long.js):

> A Long class for representing a 64 bit two's-complement integer value derived
> from the [Closure Library](https://github.com/google/closure-library)
> for stand-alone use and extended with unsigned support.

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

* [__zapph/purescript-longs__](https://github.com/zapph/purescript-longs) [LICENSE](./LICENSE.ZAPGroup)
* [__dcodeIO/long.js__](https://github.com/dcodeIO/long.js) [LICENSE](./LICENSE.dcodeIO)

This should be considered the successor package to __purescript-longs__,
which stopped being maintained in 2019.
The module names and type names have been reorganized, and the __long.js__
dependency has been inlined instead of declared in
the `dependencies` of a `package.json`.

