# Benchmark

To build `bench` you will need to install dependencies with:

```sh
cabal update
```

Build with:

```sh
cabal build
```

Run with:

```sh
time ./dist-newstyle/build/x86_64-linux/ghc-9.6.6/points-1.0.0/x/bench/build/bench/bench -w 39 -h 32 -n 10000 -s 7
```
