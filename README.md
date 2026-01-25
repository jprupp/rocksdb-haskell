This library provides Haskell bindings to
[RocksDB](http://rocksdb.org)

## Development

### Stack

```bash
stack build
stack test
```

### Nix

```bash
nix develop
cabal build all
cabal test all
```

The shell includes GHC 9.8.4, cabal, haskell-language-server, fourmolu,
hlint, and hoogle.
