# base16

[![](https://img.shields.io/hackage/v/ppad-base16?color=blue)](https://hackage.haskell.org/package/ppad-base16)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-base16-lightblue)](https://docs.ppad.tech/base16)

Pure base16 encoding & decoding on strict ByteStrings.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Data.ByteString.Base16 as B16
  >
  > -- simple base16 encoding and decoding
  > B16.encode "hello world"
  "68656c6c6f20776f726c64"
  >
  > B16.decode "68656c6c6f20776f726c64"
  Just "hello world"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/base16](https://docs.ppad.tech/base16).

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code. We could go slightly faster by using direct allocation and writes,
but we get pretty close to the best impure versions with only builders.

Current benchmark figures on 1kb inputs on a relatively-beefy NixOS VPS look
like (use `cabal bench` to run the benchmark suite):

```
  benchmarking ppad-base16
  time                 2.997 μs   (2.988 μs .. 3.009 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 3.024 μs   (3.012 μs .. 3.035 μs)
  std dev              39.14 ns   (36.12 ns .. 42.87 ns)
  variance introduced by outliers: 11% (moderately inflated)

  benchmarking ppad-base16
  time                 599.0 ns   (597.3 ns .. 601.3 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 597.3 ns   (596.3 ns .. 598.5 ns)
  std dev              3.493 ns   (2.623 ns .. 5.111 ns)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-base16
```

to get a REPL for the main library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
