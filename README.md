# ppad-base16

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

The aim is best-in-class performance. Current benchmark figures on 1kb
inputs on an M4 Silicon MacBook Air, where we avail of hardware
acceleration via ARM NEON intrinsics, look like (use `cabal bench` to
run the benchmark suite):

```
  benchmarking ppad-base16/encode
  time                 60.45 ns   (60.29 ns .. 60.65 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 60.59 ns   (60.42 ns .. 60.78 ns)
  std dev              587.6 ps   (465.2 ps .. 776.7 ps)

  benchmarking ppad-base16/decode
  time                 76.03 ns   (75.93 ns .. 76.15 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 76.04 ns   (75.94 ns .. 76.16 ns)
  std dev              373.0 ps   (285.3 ps .. 555.3 ps)
```

You should compile with the 'llvm' flag for maximum performance.

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
