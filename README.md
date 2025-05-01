Just for fun. I'll probably nuke this later.

The implementations of Sid's and Archer's algorithms can be found in `src/Sid.hs` and `src/Archer.hs` respectively.

To profile them, one can run, e.g., the following shell script:

```sh
for i in 5 10 15; do
    for j in Sid Archer; do
        echo "100 iterations of ${j}'s algorithm with N = ${i}:"
        cabal run degen-bench --enable-profiling -- "$i" "$j" 100 +RTS -sstderr >/dev/null
    done
done
```
