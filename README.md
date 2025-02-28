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

Current benchmark figures on 1kb inputs on my mid-2020 MacBook Air look
like (use `cabal bench` to run the benchmark suite):

```
  benchmarking encode/ppad-base16
  time                 5.929 μs   (5.847 μs .. 6.013 μs)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 5.975 μs   (5.913 μs .. 6.057 μs)
  std dev              233.1 ns   (172.4 ns .. 310.0 ns)

  benchmarking ppad-base16
  time                 2.510 μs   (2.454 μs .. 2.590 μs)
                       0.994 R²   (0.989 R² .. 0.997 R²)
  mean                 2.599 μs   (2.543 μs .. 2.671 μs)
  std dev              216.3 ns   (175.9 ns .. 272.8 ns)
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
