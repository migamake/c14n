# c14n

![GitHub](https://img.shields.io/github/license/mbg/c14n)
![Haskell CI](https://github.com/mbg/c14n/workflows/Haskell%20CI/badge.svg?branch=master)
![stackage-nightly](https://github.com/mbg/c14n/workflows/stackage-nightly/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/c14n)](https://hackage.haskell.org/package/c14n)

Haskell bindings for the c14n implementation in libxml (XML canonicalisation). Unfortunately there is (at the time of writing) no pure Haskell implementation, so this seems like the best option.

## Requirements

You need to have `libxml2` installed, including on macOS (with `brew install libxml2`). Then build with `stack build`. 

## Usage

The `Text.XML.C14N` module exports a function named `c14n` which provides a mid-level interface to the canonicalisation function from `libxml2`. It will handle most of the marshalling, but not much more. For example:

```haskell
> c14n [] c14n_1_1 [] False Nothing "<root>foo<!-- comment -->bar</root>" 
"<root>foobar</root>"
```

The arguments, in order, are:

* Parser options, see the module documentation or the [libxml2 documentation](http://xmlsoft.org/html/libxml-parser.html)
* The canonicalisation specification to use (`c14n_1_0`, `c14n_exclusive_1_0`, or `c14n_1_1`).
* A (potentially empty) list of namespace prefixes which is used when the canonicalisation specification used is `c14n_exclusive_1_0`
* A boolean value indicating whether to keep comments in the output or not.
* An XPath location path used to select a set of nodes that should be included in the canonicalised result
