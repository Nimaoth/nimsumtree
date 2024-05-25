# nimsumtree

_Still in progress, some things are still missing._

This library provides a tree data structure similar to a B-tree,
but each node stores user defined summaries of it's children.

Trees are immutable and it uses atomic RC so that trees can be shared across threads,
but if only one reference to a node exists it will be mutated instead.

It also provides a rope based on the sum tree, which is suitable for e.g. representing file contents in text editors.

The implementation is inspired by sum trees in [Zed](https://github.com/zed-industries/zed)

## Examples

```nim
type Count* = distinct int
type Max* = distinct int

type TestSummary* = object # Summary which is stored on each node
  count*: Count
  max*: Max

# ... implement some functions for TestSummary, Count and Max to conform to some concepts
# and associate TestSummary with the type `int`

# Create a sum tree storing ints, with two children per node, and add the numbers 1, 3, and 5
# This uses TestSummary implicitly because int.summaryType is TestSummary
let tree = SumTree[int, 2].new(@[1, 3, 5])

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
