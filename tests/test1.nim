import std/[unittest, enumerate]
import nimsumtree
import test/test_summary

test "Create empty sumtree":
  let tree = SumTree[int].new([])
  check tree.isLeaf
  check tree.height == 0
  check tree.summary == TestSummary.default
  check tree.toSeq == newSeq[int]()

  for k, item in enumerate(tree.items):
    check item == k + 1

test "Create sumtree with n numbers":
  var numbers: seq[int]
  for i in 1..100:
    numbers.add i

    let tree = SumTree[int].new(numbers)
    check tree.summary == TestSummary(count: i.Count, max: i.Max)
    check tree.toSeq == numbers

    for k, item in enumerate(tree.items):
      check item == numbers[k]
