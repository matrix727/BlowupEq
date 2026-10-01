# BlowupEq

Wolfram implementation of the highest-basis blowup-equation recursion for refined closed BPS
invariants and Wilson-loop BPS invariants.

The first validated target is local `P2`, translated from the reference notebooks and the draft
paper in this workspace. The code is stateful but model-driven: install a classical geometry
association, then query closed or Wilson-loop sectors.

## Quick Start

```bash
wolframscript -file examples/local_p2.wls
```

Run the current smoke tests:

```bash
wolframscript -file tests/local_p2_smoke.wls
```

## C++ local P2 closed benchmark

The first C++ extraction target is the closed local `P2` sector without Wilson loops.
It specializes the current blowup recursion and exact Laurent-polynomial highest-basis
decomposition to one Kähler parameter.

```bash
make build/p2_closed_bps
./build/p2_closed_bps --degree 30 --quiet
```

For a legacy C++ comparison using the previous sparse-polynomial path:

```bash
make build/p2_closed_bps_legacy
./build/p2_closed_bps_legacy --degree 30 --quiet
```

On this machine, the current optimized C++ run versus the legacy C++ sparse path:

| degree | current seconds | legacy seconds | speedup |
| --- | ---: | ---: | ---: |
| 30 | 1.48 | 2.07 | 1.40x |
| 42 | 37.93 | 53.09 | 1.40x |

The canonical C++ BPS output agrees with the legacy path through degree `42`.
For degree `42`, both output files were `190115` bytes with SHA-256
`a45f42e9ef08a7b0db01bcd177031e8ff32ab4f90ce513007a6a219b49c139be`.

Profiling higher degrees:

```bash
./build/p2_closed_bps --degree 42 --quiet --profile
```

In the same workload, the C++ degree-42 cumulative time was `37.79s`. The heavy
single-degree steps were:

| degree | seconds | sectors |
| --- | ---: | ---: |
| 30 | 0.93 | 743 |
| 33 | 2.20 | 1110 |
| 36 | 4.82 | 1595 |
| 39 | 9.89 | 2224 |
| 42 | 19.41 | 3019 |

For detailed hotspot profiling:

```bash
./build/p2_closed_bps --degree 42 --quiet --profile-detail
```

The main optimized degree-42 low-level hotspots are `fTilde` basis construction
(`22.46s`), sparse polynomial multiplication (`8.86s`), and scaled insertion
(`6.21s`). The optimized path keeps the same recursion but uses general sparse
polynomial improvements: interval-difference basis construction, ordered merge
insertion, sorted flat multiplication for medium products, and hash-based caches.

## C++ local P2 Wilson-loop run

The same C++ executable also contains the one-parameter Wilson-loop recursion
translated from `WTildeFunction1`, `SolveLambda`, `LFunctionCore`, and
`SolveWilsonLoop`.

```bash
make build/p2_closed_bps
./build/p2_closed_bps --wilson --max-r 1 --degree 41
```

Here `--degree 41` means `beta < 42`. The validated `r = 1` output is saved at
`results/p2_wilson_bps_r1_beta_lt42.txt`; its SHA-256 is
`7de9771cb889aeb5d38b205467ab6437969aa9a852a1554f83ceb9da800e96b1`.

The `r = 1` canonical output agrees byte-for-byte with the legacy C++ sparse path.
On this machine the current path took `24.99s`, while the legacy path took `34.95s`
for the full printed `beta < 42` run. A quiet profiled `r <= 3` check also ran
successfully:

| max r | total seconds | Wilson r=1 | Wilson r=2 | Wilson r=3 |
| --- | ---: | ---: | ---: | ---: |
| 2 | 33.36 | 6.62 | 8.41 | - |
| 3 | 45.33 | 6.64 | 8.37 | 11.95 |

Use:

```bash
make benchmark-p2-wilson-r1
```

for the `r = 1`, `beta < 42` timing check.

## Wolfram API

```wl
Get["src/BlowupBPS.wl"];

BlowupBPS`SetBlowupModel[BlowupBPS`LocalP2Model[]];

BlowupBPS`BPSList[{3}]
BlowupBPS`WilsonLoopBPSList[{3}, {1}]
```

The geometry input is an association with:

- `"DimFlux"` and `"DimBeta"`
- `"TripleIntersection"` tensor `a_ijk`
- `"LinearB"` and `"LinearBPrime"`
- `"CartanMatrix"` and `"R0Vector"`, defining `R(n) = CartanMatrix.n + R0Vector/2`
- `"BField"`
- optional `"InitialWilsonInvariants"`

`LocalF0Model[]` and `LocalF1Model[]` are present only as placeholders. `SetBlowupModel`
refuses them by default because their classical input still needs to be supplied and checked.
