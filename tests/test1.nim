import std/[unittest, enumerate]
import nimsumtree/sumtree
import test/test_summary

var a: Node[int, 5]
var b: SumTree[int, 5] = SumTree[int, 5].new()

test "Create empty sumtree":
  let tree = SumTree[int, treeChildrenMax].new([])
  check tree.isLeaf
  check tree.height == 0
  check tree.summary == TestSummary.default
  check tree.toSeq == newSeq[int]()

  let leftLeaf {.cursor.} = tree.leftmostLeaf.get
  check leftLeaf == newLeaf[int, treeChildrenMax]()
  let rightLeaf {.cursor.} = tree.rightmostLeaf.get
  check rightLeaf == newLeaf[int, treeChildrenMax]()

  for k, item in enumerate(tree.items):
    check item == k + 1

test "Create sumtree with n numbers":
  var numbers: seq[int]
  for i in 1..100:
    numbers.add i

    let tree = SumTree[int, treeChildrenMax].new(numbers)
    check tree.summary == TestSummary(count: i.Count, max: i.Max)
    check tree.toSeq == numbers

    for k, item in enumerate(tree.items):
      check item == numbers[k]
