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

The aim is best-in-class performance.

Current benchmark figures on 1kb inputs on my M4 MacBook Air look like
(use `cabal bench -fllvm` to run the benchmark suite):

```
  benchmarking ppad-base16/encode
  time                 295.9 ns   (295.4 ns .. 296.4 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 296.8 ns   (296.4 ns .. 297.2 ns)
  std dev              1.367 ns   (1.181 ns .. 1.619 ns)

  benchmarking ppad-base16/decode
  time                 270.8 ns   (270.6 ns .. 271.1 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 270.9 ns   (270.7 ns .. 271.1 ns)
  std dev              627.8 ps   (515.3 ps .. 766.1 ps)
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
