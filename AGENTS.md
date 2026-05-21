# inland — agent guide

## Build & run

```sh
sbt test            # all modules
sbt "project types; test"
sbt "project allocator; test"
sbt "testOnly com.nickrobison.inland.collections.NativeVectorTest"
sbt "testOnly -- -z insert"   # single test by substring
```

sbt 1.12.10, Scala 3.8.3. No scalafmt, no CI, no pre-commit.

## Modules

| dir | artifact | depends on | description |
|-----|----------|------------|-------------|
| `types/` | types | — | Refined types via Iron: `Bytes`, `Alignment`, `Offset`, `Count`, `Aligned` |
| `allocator/` | allocator | — | `NativeAllocator` trait, `HeapAllocator`, `ArenaAllocator`, `Layout[A]` typeclass |
| `allocator-laws/` | allocator-laws | allocator | discipline/cats law definitions for allocator + layout |
| `allocator-tests/` | allocator-tests | allocator, allocator-laws | runs law checks against both allocators |
| `executor/executor/` | executor | — | `VectorBatch[F[_], A]` trait (generic batch vector ops) + `Array` instance |
| `collections/collections/` | collections | allocator | `NativeVector[A]` — off-heap mutable vector backed by `NativeAllocator` |

Root aggregates all. Subproject dirs nest one extra level (`executor/executor/`, `collections/collections/`).

## Import patterns

```scala
import com.nickrobison.inland.allocator.instances.given        // Layout[Int], Layout[Double], etc.
import com.nickrobison.inland.executor.instances.array.given    // VectorBatch[Array, A]
```

## Known bugs (documented in tests)

| # | file | symptom |
|---|------|---------|
| 1 | `NativeVector.scala:74` | `checkWithinBounds` allows read at `idx == currentSize` (off-by-one) |
| 2 | `NativeVector.scala:17` | `insert` overwrites instead of shifting elements right |
| 3 | `NativeVector.scala:26` | `remove` on last element computes out-of-bounds `srcOffset` |
| 4 | `NativeVector.scala:83` | zero-init `Double` vector reallocates on every `addOne` (`ensureSize` logic flaw) |
| 5 | `NativeVector.scala:60` | iterator `next()` after exhaustion reads garbage instead of throwing `NoSuchElementException` |

## Allocator quirks

- `HeapAllocator` uses `ByteBuffer.allocateDirect` — **off-heap**, despite the name (guarantees alignment for `ValueLayout` access).
- `alignedSize` floors at 8 bytes minimum.
- `ArenaAllocator` uses slab-based arena. Constructor takes slab size + mutable `ArrayBuffer[MemorySegment]`.

## Layout typeclass

Uses `setAtIndex`/`getAtIndex` with **index** offset (not byte offset). But `NativeVector` computes **byte** offsets for `MemorySegment.copy`. Available instances: `Int`, `Double`, `Long`, `Float`, `Byte`, `Char`.

## Test frameworks

ScalaTest 3.2.20, ScalaCheck 1.17.1, Discipline 1.7.0, Cats 2.13.0.

- `types`: `AnyFunSuite` + `Checkers` (property) tests
- `allocator-tests`: discipline law checks (`AllocatorLaws`, `LayoutLaws`) via `FunSuiteDiscipline`
- `executor`: discipline + `AnyFunSuite`
- `collections`: `AnyFunSuite` unit tests + `NativeVectorCorrectnessTest` (reads raw `MemorySegment` to verify byte-level correctness) + discipline law tests via `NativeVectorLaws`

`NativeVectorCorrectnessTest` accesses `v.storage` directly (package-private) to verify bytes independently of the `Layout` read path.
