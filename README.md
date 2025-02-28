# base16

[![](https://img.shields.io/hackage/v/ppad-base16?color=blue)](https://hackage.haskell.org/package/ppad-base16)
![](https://img.shields.io/badge/license-MIT-brightgreen)

A pure implementation of base16 encoding & decoding on strict
ByteStrings.

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
code. We could go slightly faster by direct allocation and writes, but
we get pretty close to the best impure versions with only builders.

Current benchmark figures on 1kb inputs on a relatively-beefy NixOS VPS look
like (use `cabal bench` to run the benchmark suite):

```
  benchmarking encode/ppad-base16
  time                 7.634 μs   (7.543 μs .. 7.749 μs)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 7.693 μs   (7.622 μs .. 7.768 μs)
  std dev              240.6 ns   (196.5 ns .. 324.8 ns)

  benchmarking ppad-base16
  time                 1.893 μs   (1.871 μs .. 1.919 μs)
                       0.998 R²   (0.998 R² .. 0.999 R²)
  mean                 1.897 μs   (1.871 μs .. 1.924 μs)
  std dev              91.64 ns   (74.26 ns .. 118.2 ns)
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
