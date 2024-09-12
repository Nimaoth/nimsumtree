# nimsumtree

This library provides three things:
- A generic threadsafe [sumtree](scr/nimsumtree/sumtree.nim), tree data structure similar to a B-tree, but each node stores user defined summaries of it's children.
  Trees are immutable and it uses atomic RC so that trees can be shared across threads, but if only one reference to a node exists it will be mutated instead.
- A [rope](src/rope.nim) based on the sumtrees, for storing text and allowing efficient editing of large strings. Intended for e.g. text editors.
- A [CRDT](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type) [text buffer](src/buffer.nim), based on the rope and sumtree, for allowing collaborative editing of a text buffer.

The implementation is inspired by [Zed](https://github.com/zed-industries/zed)

## Examples

### Sumtree
```nim
import nimsumtree/sumtree

type Count* = distinct int
type Max* = distinct int

type TestSummary* = object # Summary which is stored on each node
  count*: Count
  max*: Max

# ... implement some functions for TestSummary, Count and Max to conform to some concepts
# and associate TestSummary with the type `int`

# Create a sum tree storing ints, with two children per node, and add the numbers 1, 3, and 5
# This uses TestSummary implicitly because int.summaryType is TestSummary
let tree = SumTree[int].new(@[1, 3, 5])

echo tree # Arc(_3, #1, Internal(h: 1, (count: 3, max: 5), children: 2))

echo tree.pretty    # Internal(_3, #1, (count: 3, max: 5), 1):
                    #   Leaf(_1, #1, (count: 2, max: 3), (1, 3))
                    #   Leaf(_2, #1, (count: 1, max: 5), (5))

echo tree.summary   # (count: 3, max: 5)

# Create a cursor to move along the tree and accumulate the Count and Max values of the summaries
# (what a cursor accumulates can be a subset of the summary, or the summary itself)
# var _ = tree.initCursor               # Accumulate nothing
# var _ = tree.initCursor Count         # Accumulate only Count
# var _ = tree.initCursor (Count, Max)  # Accumulate Count and Max
# var _ = tree.initCursor TestSummary   # Accumulate the entire TestSummary

var c = tree.initCursor (Count, Max)  # Accumulate Count and Max
assert c.seekForward(1.Count, Right)
echo c.itemSummary  # some((count: 1, max: 3))
echo c.startPos  # (1, 1): (Count, Max)
echo c.endPos  # (2, 3): (Count, Max)
```

### Rope
```nim
import nimsumtree/rope
# Note tha $ for rope is slow and should not be used regularly, this is only for demonstration
var a = Rope.new("Hello world!")
check $a == "Hello world!"
a.replace((6, 11), "you")
check $a == "Hello you!"

let b: Rope = a.slice(6, 10)
check $b == "you!"
```

### Rope
```nim
import nimsumtree/[buffer, rope]
const initialContent = "Hello world!"

# doc1 and doc2 could be on separate computers
var doc1: Buffer = initBuffer(0.ReplicaId, initialContent)
var doc2: Buffer = initBuffer(1.ReplicaId, initialContent)

let ops1 = doc1.edit([(6..<12, "?")])
let ops2 = doc2.edit([(6..<11, "you")])

assert $doc1.visibleText == "Hello ?"
assert $doc2.visibleText == "Hello you!"

doc1.applyRemote(@[ops2])
doc2.applyRemote(@[ops1])

assert $doc1.visibleText == "Hello you?"
assert $doc2.visibleText == "Hello you?"

```
